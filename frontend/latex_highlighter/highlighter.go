package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"regexp"
	"strings"
  "strconv"
)

var isfirstline bool = true
var issnippet bool = false
var verbatim []string

var plnaccum int = 0
var LINEN int = 1
var printedtabular = false

var BRACKETOPEN string = `##4rj309fn43uinfu5y4hfg784h##`
var BRACKETCLOSE string = `##908456h4j5b34y823y41g2sx##`
var SQHTML string = `##mg8204nacto-6l3jfgyu2f4cab##`
var OURSLASH string = `##236546hb234nmbjwg3h##`
var SQBCLOSE string = `##92j5m10t8934h5jkf##`
var SQBOPEN string = `##oiertklvcxapor094##`
var STARTPARAM string = boldStr("<")
var ENDPARAM string = boldStr(">")
var LIFTAPOS string = colorStr(boldStr("'"), MAROON)
var FORMATOFFSET string = boldStr(SQBOPEN) + `$offset` + boldStr("|") + `$default` + boldStr(SQBCLOSE)
var SLICE string = boldStr(SQBOPEN+":") + `$sliceexpr` + boldStr(SQBCLOSE)
var NODFLT string = boldStr(SQBOPEN) + `$offset` + boldStr("|"+SQBCLOSE)
var NOWNOW string = boldStr(SQBOPEN + "now" + SQBCLOSE)
var FORMAT string = colorStr(boldStr("format $format"), "1944a8")
var SPREAD string = formatInner("spread $spread")
var RETURN string = formatInner("return ") + formatId("$retstream") +
	formatInner(" when ") + formatId("$stopstream")
var LIBRARY string = boldStr("library ") +
	colorStr(boldStr("$libname"), GOLD)
var CONST string = boldStr("const ") + "$name" + boldStr("=") + "$rest"
var DATA string = boldStr("$kind ") + formatType("$name") + boldStr("=") + "$rest"
var USE string = boldStr("use $kind ") + colorStr(boldStr("$name"), GOLD)
var USEINNER string = boldStr("use innerspec ") + formatInner("$name")
var LITCURLYOPEN string = boldStr("\\{")
var LITCURLYCLOSE string = boldStr("\\}")

var INNERCOLOR string = "4d8491"
var ORANGE string = "eb7600"
var MAROON string = "c40000"
var GOLD string = "e69e22"
var RWSTYLE string = "1c21b8"

var rwMap map[string]string = map[string]string{
	"let":      colorStr("let", RWSTYLE),
	"in":       colorStr("in", RWSTYLE),
	"where":    colorStr("where", RWSTYLE),
	"if":       colorStr("if", RWSTYLE),
	"then":     colorStr("then", RWSTYLE),
	"else":     colorStr("else", RWSTYLE),
	"deriving": boldStr("deriving"),
}

func boldStr(s string) string {
	return OURSLASH + "textbf" + BRACKETOPEN + s + BRACKETCLOSE
}

func colorStr(s, color string) string {
	return OURSLASH + "textcolor" + SQHTML + BRACKETOPEN + color + BRACKETCLOSE + BRACKETOPEN + s + BRACKETCLOSE
}

func formatInner(s string) string {
	return colorStr(boldStr(s), INNERCOLOR)
}

func formatType(s string) string {
	return colorStr(boldStr(s), "008000")
}

func formatId(s string) string {
	return colorStr(boldStr(s), ORANGE)
}

func formatArgId(s string) string {
	return colorStr(boldStr(s), "96325d")
}

func formatArity(b byte) string {
	return colorStr(string([]byte{b}), MAROON)
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
		parseError("No stream name: " + s)
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
	re := regexp.MustCompile(`^(?P<kind>output|define) (?P<lhs>([^=]|=>)*)=(?P<rhs>([^>].*$|$))`)
	themap := FindStringSubmatchMap(re, s)
	ret += boldStr(themap["kind"]) + " "
	body := themap["rhs"]
	s = themap["lhs"]
	nspaces := len(s) - strings.LastIndexFunc(s, func(r rune) bool { return r != ' ' }) - 2
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
	if nspaces > 1 {
		ret += OURSLASH + "phantom" + BRACKETOPEN + strings.Repeat(".", nspaces-1) + BRACKETCLOSE
	}
	ret += boldStr("=") + body
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
	fkind := boldStr("input")
	return (fkind + " " + formatType(typ) + " " + fid)
}

func processInnerSpecDeclaration(s string) string {
	if !strings.HasPrefix(s, "innerspec ") {
		return s
	}
	re := regexp.MustCompile(`^innerspec (?P<rest>.*)`)
	themap := FindStringSubmatchMap(re, s)
	ret := boldStr("innerspec ")
	s = themap["rest"]
	nspaces := len(s) - strings.LastIndexFunc(s, func(r rune) bool { return r != ' ' }) - 2
	strs := strings.SplitN(s, "<", 2)
	s = strs[0]
	id, typConstr := getLastWord(s)
	ret += formatType(typConstr)
	ret += " "
	ret += formatInner(id)
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
	if nspaces > 0 {
		ret += OURSLASH + "phantom" + BRACKETOPEN + strings.Repeat(".", nspaces) + BRACKETCLOSE
	}
	return ret
}

func processFormatDeclaration(s string) string {
	formatRE := regexp.MustCompile(`^format (?P<format>(JSON|CSV))$`)
	return formatRE.ReplaceAllString(s, FORMAT)
}

func processSpread(s string) string {
	spreadRE := regexp.MustCompile(`^spread (?P<spread>(.*))$`)
	return spreadRE.ReplaceAllString(s, SPREAD)
}

func processReturn(s string) string {
	returnRE := regexp.MustCompile(`^return (?P<retstream>(.*)) when (?P<stopstream>(.*))$`)
	return returnRE.ReplaceAllString(s, RETURN)
}

func processFileTypeDeclaration(s string) string {
	libraryRE := regexp.MustCompile(`^library (?P<libname>.*)$`)
	return libraryRE.ReplaceAllString(s, LIBRARY)
}

func processUsage(s string) string {
	useRE := regexp.MustCompile(`^use (?P<kind>(library|theory|haskell)) (?P<name>.*)$`)
	useinnerRE := regexp.MustCompile(`^use innerspec (?P<name>.*)$`)
	s0 := useinnerRE.ReplaceAllString(s, USEINNER)
	return useRE.ReplaceAllString(s0, USE)
}

func processConstDataDec(s string) string {
	constRE := regexp.MustCompile(`^const (?P<name>[^=]+)=(?P<rest>.*)$`)
	dataRE := regexp.MustCompile(`^(?P<kind>const|data|type) (?P<name>[^=]+)=(?P<rest>.*)$`)
	s = constRE.ReplaceAllString(s, CONST)
	s = dataRE.ReplaceAllString(s, DATA)
	return s
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
	streamAccessRE := regexp.MustCompile(`\[(?P<offset>-?[^\[\]]*)\|(?P<default>[^\]]+)\]`)
	streamNoDfltRE := regexp.MustCompile(`\[(?P<offset>[^|\]]+)\|\]`)
	streamNowREnow := regexp.MustCompile(`\[now\]`)
	streamSliceRE := regexp.MustCompile(`\[:(?P<sliceexpr>[^\]]+)\]`)

	s = streamNoDfltRE.ReplaceAllString(s, NODFLT)
	s = streamNowREnow.ReplaceAllString(s, NOWNOW)
	s = streamSliceRE.ReplaceAllString(s, SLICE)
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
	return strings.Replace(s, "  ", "\\phantom{..}", -1)
	// spacesRE := regexp.MustCompile(` (?P<spaces> +)$`)
	// themap := FindStringSubmatchMap(spacesRE, s)
	// if len(themap) == 0 || len(themap["spaces"]) == 0 {
	// 	return s
	// }
	// spaces := themap["spaces"]
	// rhs := s[len(spaces):]
	// return "\\phantom{" + spaces + "}" + rhs
}

func extractVerbatim(s string) string {
	lhs := ""
	rhs := s
	ix := strings.Index(rhs, "VERB")
	for ix != -1 {
		lhs = lhs + rhs[:ix]
		stopchar := rhs[ix+4]
		rhs = rhs[ix+5:]
		stopix := strings.Index(rhs, string(stopchar))
		verbix := len(verbatim)
		verbatim = append(verbatim, rhs[:stopix])
    sverbix := strconv.Itoa(verbix)
    if verbix < 10 {
      sverbix = "0" + sverbix
    }
		lhs = lhs + "VERBATIM" + sverbix
		rhs = rhs[stopix+1:]
		ix = strings.Index(rhs, "VERB")
	}
	return lhs + rhs
}

func restoreVerbatim(s string) string {
  re := regexp.MustCompile("VERBATIM..")
  s = re.ReplaceAllStringFunc(s, func(target string) string {
    ix,_ := strconv.Atoi(target[8:])
    return verbatim[ix]})
	return s
}

func process(s string) {
  s = extractVerbatim(s)
	s = liftFuns(s)
	s = processInputDeclaration(s)
	s = processInnerSpecDeclaration(s)
	s = processOutputDeclaration(s)
	s = processConstDataDec(s)
	s = processFormatDeclaration(s)
	s = processFileTypeDeclaration(s)
	s = processSpread(s)
	s = processReturn(s)
	s = processUsage(s)
	s = processAccesses(s)
	s = processReservedWords(s)
	// s = html.EscapeString(s)
	s = latexFormatting(s)
	s = strings.Replace(s, BRACKETOPEN, "{", -1)
	s = strings.Replace(s, BRACKETCLOSE, "}", -1)
	s = strings.Replace(s, SQBOPEN, "[", -1)
	s = strings.Replace(s, SQBCLOSE, "]", -1)
	s = strings.Replace(s, OURSLASH, "\\", -1)
	s = strings.Replace(s, SQHTML, "[HTML]", -1)
	println(s)
}

func latexFormatting(s string) string {
	s = strings.Replace(s, "\\", "\\textbackslash ", -1)
	s = strings.Replace(s, "{", LITCURLYOPEN, -1)
	s = strings.Replace(s, "}", LITCURLYCLOSE, -1)
	s = preserveLeadingSpaces(s)
	s = strings.Replace(s, "_", "\\_", -1)
	s = strings.Replace(s, "$", "\\$", -1)
	s = strings.Replace(s, "%", "\\%", -1)
	s = strings.Replace(s, "&", "\\&", -1)
	s = strings.Replace(s, "^", "\\^{}", -1)
	return s
}

func emitln(s string) {
  emit(s + "\n")
}

func emit(s string) {
	s = strings.Replace(s, " ", "\\phantom{x}", -1)
	fmt.Print(s)
}

func printprefix() {
	emitln("\\noindent{\\frontendsize\\tt")
  printedtabular = false
  plnaccum = 0
  isfirstline = true
}

func printsuffix() {
  if issnippet {
    emitln("}")
  } else {
		emitln(`\\`)
		emitln(`\multicolumn{2}{X}{\color[HTML]{888888}\rule[2mm]{30mm}{.1pt}}`)
		emitln("\\end{tabularx}}")
  }
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	verbatim := false
  printprefix()
	for scanner.Scan() {
		txt := scanner.Text()
		if strings.HasPrefix(txt, "LINEN ") {
      linens := txt[6:]
      LINEN,_ = strconv.Atoi(linens)
			continue
		}
		if txt == "SNIPPET" {
			issnippet = true
			continue
		}
    if txt == "BREAK" {
      printsuffix()
      emitln("")
      printprefix()
      continue
    }
		if !issnippet && !printedtabular {
			printedtabular = true
			emitln("\\begin{tabularx}{\\textwidth}{@{}r@{\\hspace{.5em}}l}")
			emitln(`\multicolumn{2}{X}{\color[HTML]{888888}\rule[2mm]{30mm}{.1pt}}\\`)
		}
		// LOGICAL DIFF
		if txt == "#ENDOFHASKELL" {
			verbatim = false
			println("\\textcolor[HTML]{606060}{\\#ENDOFHASKELL}")
			continue
		}
		if txt == "#HASKELL" {
			verbatim = true
			println("\\textcolor[HTML]{606060}{\\#HASKELL}")
			continue
		}
		if verbatim {
			// s := html.EscapeString(txt)
			if len(strings.TrimSpace(txt)) == 0 {
				txt = ""
			} else {
				txt = "\\textcolor[HTML]{606060}{" + latexFormatting(txt) + "}"
			}
			println(txt)
			continue
		}
		if strings.HasPrefix(txt, "--") {
      println(`\textcolor[HTML]{606060}{` + "-{}-" + latexFormatting(txt[2:]) + `}`)
			continue
		}
		process(txt)
	}
  printsuffix()
}

func println(s string) {
	if len(strings.TrimSpace(s)) > 0 {
		if plnaccum == 0 {
			if !isfirstline {
				emitln("\\\\")
			} else {
				isfirstline = false
			}
		} else {
      emitln(fmt.Sprintf("\\\\[%.1fem]", float64(plnaccum)*0.5))
		}
		for i := 0; i < plnaccum; i++ {
			emitln("%")
		}
    if !issnippet {
      emit("\\linensize{\\color[HTML]{4d798f}")
      emit(strconv.Itoa(LINEN))
      emit("}&")
    }
    s = restoreVerbatim(s)
		emit(s)
    LINEN = LINEN + 1
		plnaccum = 0
	} else {
		plnaccum = plnaccum + 1
	}
}
