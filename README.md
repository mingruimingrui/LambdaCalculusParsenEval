# LambdaCalculusParsenEval
#
#
#
# //Scala installation guide
#
# To install scala 2.11, go to http://scala-ide.org/download/sdk.html
#
# Do note that scala does not have backward compatibility and so to
# access the parsing library, please download the JAR file from
#
# http://central.maven.org/maven2/org/scala-lang/modules/scala-parser-combinators_2.11/1.0.2/
# >> scala-parser-combinators_2.11-1.0.2.jar
#
#
#
# //OVERVIEW
# 
# This is a simple parser and evaluator for lambda calculus making use
# of RegexParser.
# 
# Lots of messy/needless codes everywhere (I know) but I'd probably only be cleaning them up
# when I am free.
#
#
#
# //Abstract data type in EBNF format:
#
# term         = <variable>   |   <abstraction>   |   <application>   |   <parenthesis>
# variable     = <identifier>
# abstraction  = “\”   ++   <variable>   ++   “.”   ++   <term>
# application  = <term>   ++   “ ”   ++   [<term>]+
# parenthesis  = “(”   ++   <term>   ++   ”)”
# let          = “let”   ++   <variable>   ++   “=”   ++   <term>   ++   “in”   ++   <term>
#
#
#
# Capabilities:
# 1. Parsing capabilities for Lambda terms, application, abstraction and 'let'
# 2. Strict call-by-value evaluation
#
#
#
# Coming next...
# 1. Improved alpha-renaming capabilities to separate free from bound variables to allow for normal order evaluation.
# 2. Clean up code
# 3. Learning process
# 4. Pretty printer
