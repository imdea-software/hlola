package main

import (
	"bufio"
	"bytes"
	"fmt"
	"html"
	"os"
	"regexp"
	"strings"
)

var OUROPEN string = `||4rj309fn43uinfu5y4hfg784h||`
var OURCLOSE string = `||908456h4j5b34y823y41g2sx||`
var OURQUOTE string = `||236546hb234nmbjwg3h||`
var OURSPACE string = `||981xnh-5hnaoa3i60gend||`
var INPUT string = formatStr("input", BOLD)
var HIDDEN string = formatStr("hidden", BOLD)
var OUTPUT string = formatStr("output", BOLD)
var STARTPARAM string = formatStr(OUROPEN, BOLD)
var ENDPARAM string = formatStr(OURCLOSE, BOLD)
var LIFTAPOS string = formatStr("'", MAROON+BOLD)
var FORMATOFFSET string = formatStr("[", BOLD) + `$offset` +
	formatStr("|", BOLD) + `$default` + formatStr("]", BOLD)
var NOWZERO string = `[ZERO|]`
var NOWNOW string = formatStr("[now]", BOLD)
var FORMAT string = formatStr("format $format", "color:#1944a8;"+BOLD)
var LIBRARY string = formatStr("library ", BOLD) +
	formatStr("$libname", BOLD+GOLD)
var USE string = formatStr("use $kind ", BOLD) +
	formatStr("$name", BOLD+GOLD)
var BOLD string = "font-weight:bold;"
var ORANGE string = "color:#eb7600;"
var MAROON string = "color:#c40000;"
var GOLD string = "color:#e69e22"
var RWSTYLE string = "color:#1c21b8"

var rwMap map[string]string = map[string]string{
	"let":   formatStr("let", RWSTYLE),
	"in":    formatStr("in", RWSTYLE),
	"where": formatStr("where", RWSTYLE),
	"if":    formatStr("if", RWSTYLE),
	"then":  formatStr("then", RWSTYLE),
	"else":  formatStr("else", RWSTYLE),
}

func formatStr(s, format string) string {
	return OUROPEN + `span` + OURSPACE + `style=` + OURQUOTE + format + OURQUOTE + OURCLOSE + s + OUROPEN + `/span` + OURCLOSE
}

func formatType(s string) string {
	return formatStr(s, "color:#008000;"+BOLD)
}

func formatId(s string) string {
	return formatStr(s, ORANGE+BOLD)
}

func formatArgId(s string) string {
	return formatStr(s, "color:#96325d;"+BOLD)
}

func formatArity(b byte) string {
	return formatStr(string([]byte{b}), MAROON)
}

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

func processOutputDeclaration(s string) string {
	ret := ""
	// if strings.HasPrefix(s, "hidden ") {
	// 	ret = HIDDEN + " "
	// 	s = s[len("hidden "):]
	// }
	if !strings.HasPrefix(s, "output ") && !strings.HasPrefix(s, "define ") {
		return s
	}
	// ret += OUTPUT + " "
	// We can use type constraints
	re := regexp.MustCompile(`^(?P<kind>output|define) (?P<lhs>([^=]|=>)*)=(?P<rhs>[^>].*)$`)
	themap := FindStringSubmatchMap(re, s)
	ret += formatStr(themap["kind"], BOLD) + " "
	body := themap["rhs"]
	s = themap["lhs"]
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	ret += formatType(typConstr)
	ret += " "
	ret += formatId(id)
	ret += " "
	// rest = strs[1]
	if len(strs) > 1 {
		rawParams := strings.Trim(strs[1], " ")
		rawParams = rawParams[:len(rawParams)-1]
		re := regexp.MustCompile("> *<")
		rawParams = re.ReplaceAllString(rawParams, "><")
		pars := strings.Split(rawParams, "><")
		for _, x := range pars {
			parid, typ := getLastWord(x)
			ret += STARTPARAM
			ret += formatType(typ)
			ret += " "
			ret += formatArgId(parid)
			ret += ENDPARAM
			ret += " "
		}
	}
	ret += formatStr("=", BOLD) + body
	return ret
}

func processInputDeclaration(s string) string {
	inputRE := regexp.MustCompile(`^input (?P<type>.*) (?P<id>[^ ]*)$`)
	themap := FindStringSubmatchMap(inputRE, s)
	if len(themap) == 0 {
		return s
	}
	id := themap["id"]
	typ := themap["type"]
	fid := formatId(id)
	return (INPUT + " " + formatType(typ) + " " + fid)
}

func processFormatDeclaration(s string) string {
	formatRE := regexp.MustCompile(`^format (?P<format>(JSON|CSV))$`)
	return formatRE.ReplaceAllString(s, FORMAT)
}

func processFileTypeDeclaration(s string) string {
	libraryRE := regexp.MustCompile(`^library (?P<libname>.*)$`)
	return libraryRE.ReplaceAllString(s, LIBRARY)
}

func processUsage(s string) string {
	useRE := regexp.MustCompile(`^use (?P<kind>(library|theory|haskell)) (?P<name>.*)$`)
	return useRE.ReplaceAllString(s, USE)
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
	ret := lefthand
	if hasarity {
		ret += formatArity(arity)
	}
	ret += LIFTAPOS
	return ret + liftFuns(righthand)
}

func isDigit(c byte) bool {
	return 47 < c && c < 58
}

func idchar(c byte) bool {
	return isDigit(c) || (c > 64 && c < 91) || c == 95 || (c > 96 && c < 123)
}

func processAccesses(s string) string {
	streamAccessRE := regexp.MustCompile(`\[(?P<offset>-?[^[]]*)\|(?P<default>[^\]]+)\]`)
	streamNowRE := regexp.MustCompile(`\[0\|\]`)
	streamNowREnow := regexp.MustCompile(`\[now\]`)
	s = streamNowRE.ReplaceAllString(s, NOWZERO)
	s = streamNowREnow.ReplaceAllString(s, NOWNOW)
	s = streamAccessRE.ReplaceAllString(s, FORMATOFFSET)
	return s
}

func processReservedWords(s string) string {
	STARTW := `([^a-zA-Z0-9_]|^)`
	ENDW := `([^a-zA-Z0-9_]|$)`
	for k, v := range rwMap {
		re := regexp.MustCompile(STARTW + k + ENDW)
		s = re.ReplaceAllString(s, "${1}"+v+"${2}")
	}
	return s
}

func preserveLeadingSpaces(s string) string {
	return strings.Replace(s, " ", "&nbsp;", -1)
}

func process(s string) {
	s = liftFuns(s)
	s = processInputDeclaration(s)
	s = processOutputDeclaration(s)
	s = processFormatDeclaration(s)
	s = processFileTypeDeclaration(s)
	s = processUsage(s)
	s = processAccesses(s)
	s = processReservedWords(s)
	s = html.EscapeString(s)
	s = preserveLeadingSpaces(s)
	s = strings.Replace(s, OUROPEN, "<", -1)
	s = strings.Replace(s, OURCLOSE, ">", -1)
	s = strings.Replace(s, OURQUOTE, "\"", -1)
	s = strings.Replace(s, OURSPACE, " ", -1)
	println(s)
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	verbatim := false
	fmt.Println(`<span style="font-family:monospace">`)
	for scanner.Scan() {
		txt := scanner.Text()
		if txt == "#ENDOFHASKELL" {
			verbatim = false
			println(txt)
			println(`</span>`)
			continue
		}
		if txt == "#HASKELL" {
			verbatim = true
			println(`<span style="color:#606060;">`) // + OURQUOTE + format + OURQUOTE + OURCLOSE + s + OUROPEN + `/span` + OURCLOSE)
			println(txt)
			continue
		}
		if verbatim {
			s := html.EscapeString(txt)
			s = preserveLeadingSpaces(s)
			println(s)
			continue
		}
		if strings.HasPrefix(txt, "--") {
			println(`<span style="color:#606060">` + html.EscapeString(txt) + `</span>`)
			continue
		}
		process(txt)
	}
	fmt.Println(`</span>`)
}

func println(s string) {
	fmt.Print(s)
	fmt.Println("<br/>")
}
