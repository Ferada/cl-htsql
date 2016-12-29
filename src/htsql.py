import htsql


application = htsql.HTSQL("sqlite:///htsql_demo.sqlite")


global y
y = None


def p(string):
    global y
    with application:
        y = htsql.core.syn.parse.parse(string)
        return y


def l(name):
    result = []
    for c in name:
        if c.isupper() and result:
            result.append("-")
        result.append(c)
    return "".join(result)


def keyword(name):
    return ":{}".format(name.upper())


def n(expression):
    name = expression.__class__.__name__
    name = name[:name.find("Syntax")]
    return keyword(l(name))


def t(expression):
    arguments = [n(expression)]
    d = expression.__dict__
    if "identifier" in d:
        arguments.append(t(expression.identifier))
    if "text" in d:
        arguments.append("\"{}\"".format(expression.text))
    if "symbol" in d:
        if not isinstance(expression, (htsql.core.syn.syntax.ProjectSyntax, htsql.core.syn.syntax.FilterSyntax, htsql.core.syn.syntax.CollectSyntax, htsql.core.syn.syntax.DetachSyntax)):
            arguments.append(expression.symbol)
    if "arm" in d:
        arguments.append(t(expression.arm))
    if "larm" in d:
        arguments.append(t(expression.larm))
    if "rarm" in d:
        arguments.append(t(expression.rarm))
    if "arms" in d:
        arguments.extend(t(x) for x in expression.arms)
    if "rarms" in d:
        arguments.extend(t(x) for x in expression.rarms)
    return "({})".format(" ".join(arguments))


# t(p("/school"))
# (:COLLECT (:IDENTIFIER "school"))

# t(p("/'school'"))
# (:COLLECT (:STRING "school"))

# http://localhost:8080/school?(count(program.title~'Engi')>0&count(department.code~'a')>0)/:sql

# -> /school?exists(program.title~'Engi') would be nice

# SELECT "school"."code",
#        "school"."name",
#        "school"."campus"
# FROM "school"
#      LEFT OUTER JOIN (SELECT COUNT(NULLIF(("program"."title" LIKE '%Engi%' ESCAPE '\'), 0)) AS "count",
#                              "program"."school_code"
#                       FROM "program"
#                       GROUP BY 2) AS "program"
#                      ON ("school"."code" = "program"."school_code")
#      LEFT OUTER JOIN (SELECT COUNT(NULLIF(("department"."code" LIKE '%a%' ESCAPE '\'), 0)) AS "count",
#                              "department"."school_code"
#                       FROM "department"
#                       GROUP BY 2) AS "department"
#                      ON ("school"."code" = "department"."school_code")
# WHERE (COALESCE("program"."count", 0) > 0)
#       AND (COALESCE("department"."count", 0) > 0)
# ORDER BY 1 ASC
