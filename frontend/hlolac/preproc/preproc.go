package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strings"
)

type TypeId struct {
	id  string
	typ string
}

type TypeAndDef struct {
	id          string
	typ         string
	def         string
	template    string
	params      []TypeId
	constraints string
	hidden      bool
}

type HeaderInfo struct {
	libname  string
	format   string
	imports  string
	verbatim string
}

type IOid struct {
	id    string
	isOut bool
}

var outwriter *bufio.Writer
var outpath string = ""
var outfname string = "/Main.hs"

var typedInputs map[string]string = make(map[string]string)
var typedOutputs map[string]TypeAndDef = make(map[string]TypeAndDef)
var lastid string = ""
var order []IOid = make([]IOid, 0)
var headerinfo = new(HeaderInfo)

var inputTemplate string = `%s :: Stream %s
%s = Input "%s"
`
var outputTemplate string = `
%s :: %s%sStream %s
%s %s= "%s" %s=: (%s)
`
var closedOutputTemplate string = `
%s :: %s%sStream %s
%s %s= "%s" %s=: (%s
`

/*
First argument: custom imports
Second argument: JSON/CSV
Third argument: custom haskell
*/
var headerMain = `{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import GHC.Generics
import Data.Aeson
import InFromFile
import System.IO
import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import DecDyn
import qualified Prelude as P
%s
main :: IO ()
main =do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpec%s False specification


-- Custom Haskell
%s
`

/*
args: Libname, custom imports, custom haskell
*/
var headerLib = `{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.%s where
import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
%s

-- Custom Haskell
%s
`

// As seen on StackOverflow
func FindStringSubmatchMap(r *regexp.Regexp, s string) map[string]string {
	captures := make(map[string]string)
	match := r.FindStringSubmatch(s)
	if match == nil {
		return captures
	}
	for i, name := range r.SubexpNames() {
		// Ignore the whole regexp match and unnamed groups
		if i == 0 || name == "" {
			continue
		}
		captures[name] = match[i]
	}
	return captures
}

func findBalanced(s string, adder rune, remover rune) (string, string) {
	typ := new(bytes.Buffer)
	rest := new(bytes.Buffer)
	reader := strings.NewReader(s)
	balancecount := 1
	ch, _, err := reader.ReadRune()
	if err != nil || ch != adder {
		panic("Balanced error")
	}
	typ.WriteRune(ch)
	for balancecount > 0 {
		ch, _, err := reader.ReadRune()
		if err != nil {
			panic("Balanced error")
		}
		switch ch {
		case adder:
			balancecount++
		case remover:
			balancecount--
		}
		typ.WriteRune(ch)
	}
	rest.ReadFrom(reader)
	return typ.String(), rest.String()
}

func munchType(s string) (string, string) {
	s = strings.TrimSpace(s)
	switch s[0] {
	case '(':
		return findBalanced(s, '(', ')')
	case '[':
		return findBalanced(s, '[', ']')
	default:
		strs := strings.SplitN(s, " ", 2)
		return strs[0], strs[1]
	}
}

func parseError(s string) {
	panic(s)
}

func getLastWord(s string) (string, string) {
	s = strings.Trim(s, " ")
	lastwordRE := regexp.MustCompile(`^(?P<rest>.*)? (?P<lastword>[^ ]+)$`)
	themap := FindStringSubmatchMap(lastwordRE, s)
	if len(themap) < 2 {
		parseError("No stream name")
	}
	// typ := themap["type"]
	return themap["lastword"], strings.Trim(themap["rest"], " ")
}

func processOutputDeclaration(s string) {
	// if strings.HasPrefix(s, "hidden ") {
	// 	hidden = true
	// 	s = s[len("hidden "):]
	// }
	if !strings.HasPrefix(s, "output ") && !strings.HasPrefix(s, "define ") {
		return
	}
	// We can use type constraints
	re := regexp.MustCompile(`^(?P<kind>output|define) (?P<lhs>([^=]|=>)*)=(?P<rhs>[^>].*)$`)
	themap := FindStringSubmatchMap(re, s)
	hidden := themap["kind"] == "define"
	body := themap["rhs"]
	s = themap["lhs"]
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	typ := typConstr
	constr := ""
	if strings.Contains(typConstr, "=>") {
		strs := strings.SplitN(typConstr, "=>", 2)
		typ = strings.TrimSpace(strs[1])
		constr = strs[0] + "=> "
	}
	// rest = strs[1]
	params := []TypeId{}
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			params = append(params, TypeId{parid, typ})
		}
	}
	typedOutputs[id] = TypeAndDef{id, typ, body, outputTemplate, params,
		constr, hidden}
	lastid = id
	order = append(order, IOid{id, true})
}

func getParamTypes(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		ret = ret + tyid.typ + " -> "
	}
	return ret
}

func getParamNames(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		ret = ret + tyid.id + " "
	}
	return ret
}

func getParamIds(params []TypeId) string {
	ret := ""
	for _, tyid := range params {
		if !strings.Contains(tyid.typ, "->") {
			// Functions are not Show. Every other paramater is supposed to be an
			// instance of Show, and will be included
			ret = ret + "<: " + tyid.id + " "
		}
	}
	return ret
}

func processInputDeclaration(s string) {
	inputRE := regexp.MustCompile(`^input (?P<type>[^ ]*) +(?P<id>[^ ]*)$`)
	themap := FindStringSubmatchMap(inputRE, s)
	if len(themap) == 0 {
		return
	}
	lastid = ""
	id := themap["id"]
	typ := themap["type"]
	typedInputs[id] = typ
	order = append(order, IOid{id, false})
	//fmt.Println(typedInputs)
}

func processFormatDeclaration(s string) {
	formatRE := regexp.MustCompile(`^format (?P<format>(JSON|CSV))$`)
	themap := FindStringSubmatchMap(formatRE, s)
	if len(themap) == 0 {
		if strings.HasPrefix(s, "format ") {
			panic("Unknown format: " + s)
		}
		return
	}
	headerinfo.format = themap["format"]
}

func processFileTypeDeclaration(s string) {
	libraryRE := regexp.MustCompile(`^library (?P<libname>.*)$`)
	themap := FindStringSubmatchMap(libraryRE, s)
	if len(themap) > 0 {
		if outwriter != nil && outpath != "" {
			panic("Library declared too late")
		}
		headerinfo.libname = themap["libname"]
		outfname = "/Lib/" + headerinfo.libname + ".hs"
	}
}

func processUsage(s string) {
	useRE := regexp.MustCompile(`^use (?P<kind>(library|theory|haskell)) (?P<name>.*)$`)
	themap := FindStringSubmatchMap(useRE, s)
	if len(themap) < 2 {
		if strings.HasPrefix(s, "use ") {
			panic("Unknown usage: " + s)
		}
		return
	}
	var usekind string
	switch themap["kind"] {
	case "library":
		usekind = "Lib."
	case "theory":
		usekind = "Theories."
	default:
		usekind = ""
	}
	newimport := "import " + usekind + themap["name"]
	headerinfo.imports += newimport + "\n"
}

func liftFuns(s string) string {
	apix := strings.IndexByte(s, '\'')
	if apix == -1 {
		return s
	}
	//it's part of a name
	if apix > 0 {
		if isDigit(s[apix-1]) {
			if apix > 1 && idchar(s[apix-2]) {
				return s[:apix+1] + liftFuns(s[apix+1:])
			}
		} else if idchar(s[apix-1]) {
			return s[:apix+1] + liftFuns(s[apix+1:])
		}
	}
	// it's a char
	if apix+2 < len(s) && s[apix+2] == '\'' {
		return s[:apix+3] + liftFuns(s[apix+3:])
	}
	// we are lifting something:
	hasarity := false
	var arity byte
	upto := apix
	if apix > 0 && isDigit(s[apix-1]) {
		hasarity = true
		arity = s[apix-1]
		upto = apix - 1
	}
	lefthand := s[:upto]
	righthand := s[apix+1:]
	funame := ""
	rest := ""
	separator := " "
	if righthand[0] == '(' {
		funame, rest = findBalanced(righthand, '(', ')')
	} else {
		indexnl := strings.Index(righthand, "\n")
		indexspace := strings.Index(righthand, " ")
		if indexnl > 0 && indexnl < indexspace {
			separator = "\n"
		}
		splitt := strings.SplitN(righthand, separator, 2)
		funame = splitt[0]
		if len(splitt) == 2 {
			rest = splitt[1]
		}
	}
	if hasarity {
		righthand = "(magic" + string([]byte{arity}) + " " + funame + ")" + separator + rest
	} else {
		righthand = "(toolLift " + funame + ")" + separator + rest
	}
	return liftFuns(lefthand + righthand)
}

func isDigit(c byte) bool {
	return 47 < c && c < 58
}

func idchar(c byte) bool {
	return isDigit(c) || (c > 64 && c < 91) || c == 95 || (c > 96 && c < 123)
}

func printToFile(format string, args ...interface{}) {
	if outwriter == nil {
		if _, err := os.Stat(outpath + outfname); err == nil {
			fmt.Printf("WARNING: Duplicated file: " + outfname + "\n")
		}
		f, err := os.Create(outpath + outfname)
		if err != nil {
			panic(err)
		}
		outwriter = bufio.NewWriter(f)
	}
	fmt.Fprintf(outwriter, format, args...)
	outwriter.Flush()
}

func printIn(id string) {
	printToFile(inputTemplate, id, typedInputs[id], id, id)
}

func printOut(id string) {
	tad := typedOutputs[id]
	def := tad.def
	streamAccessRE := regexp.MustCompile(`\[(?P<offset>-?[^[]]*)\|(?P<default>[^\]]+)\]`)
	streamNowRE := regexp.MustCompile(`\[0\|\]`)
	streamNowREnow := regexp.MustCompile(`\[now\]`)
	// Replacements:
	def = streamNowRE.ReplaceAllString(def, `:@(0, Leaf undefined)`)
	def = streamNowREnow.ReplaceAllString(def, `:@(0, Leaf undefined)`)
	def = streamAccessRE.ReplaceAllString(def, `:@($offset,$default)`)
	// Trim:
	def = strings.Trim(def, " \t")
	def = liftFuns(def)
	printToFile(tad.template, id, tad.constraints, getParamTypes(tad.params), tad.typ, id, getParamNames(tad.params), id, getParamIds(tad.params), def)
}

func process(s string) {
	if strings.HasPrefix(s, " ") {
		if lastid == "" {
			panic("No last output")
		}
		thedef := typedOutputs[lastid]
		isWhere, err := regexp.MatchString("^ *where( |$)", s)
		if err != nil {
			panic(err)
		}
		if isWhere {
			thedef.def += ")"
			thedef.template = closedOutputTemplate
		}
		thedef.def += "\n" + s
		typedOutputs[lastid] = thedef
		return
	}
	processInputDeclaration(s)
	processOutputDeclaration(s)
	processFormatDeclaration(s)
	processFileTypeDeclaration(s)
	processUsage(s)
	//fmt.Println(s)
}

func printSystem() {
	if headerinfo.libname != "" {
		return
	}
	printToFile(`specification :: Specification
specification = [`)
	fst := true
	for id, tad := range typedOutputs {
		if len(tad.params) > 0 || tad.hidden {
			continue
		}
		if !fst {
			printToFile(", ")
		}
		fst = false
		printToFile("out " + id)
	}
	printToFile("]\n")
}

func printHeader() {
	if headerinfo.libname == "" {
		printToFile(headerMain, headerinfo.imports, headerinfo.format, headerinfo.verbatim)
	} else {
		printToFile(headerLib, headerinfo.libname, headerinfo.imports, headerinfo.verbatim)
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	outwriter = bufio.NewWriter(os.Stdout)
	if len(os.Args) > 2 {
		outpath = os.Args[1]
		outwriter = nil
		infile, _ := os.Open(os.Args[2])
		defer infile.Close()
		scanner = bufio.NewScanner(infile)
	}
	verbatim := false
	for scanner.Scan() {
		txt := scanner.Text()
		if txt == "#ENDOFHASKELL" {
			verbatim = false
			continue
		}
		if txt == "#HASKELL" {
			verbatim = true
			continue
		}
		if verbatim {
			headerinfo.verbatim += txt + "\n"
			//fmt.Println(txt)
			continue
		}
		process(scanner.Text())
	}
	if headerinfo.format == "" && headerinfo.libname == "" {
		panic("No format specified")
	}
	printHeader()
	printSystem()
	for _, idout := range order {
		if idout.isOut {
			printOut(idout.id)
		} else {
			printIn(idout.id)
		}
	}
}
