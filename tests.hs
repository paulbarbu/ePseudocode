-- assign function call or expression or function def
-- return fucntion call or expression or funciton def
-- sequence of stements in complete if in simple if, in for, while, function def


runLex mainParser "func x() ret 5 sf func daca 3 atunci 5 sf daca"
runLex mainParser "func x() ret func y() 1 sffunc daca 1 atunci sfdaca sffunc"
runLex mainParser "func x() ret func y() 1 sffunc ret 1 sffunc"

runLex mainParser "func x() daca 5 atunci 3 sfdaca daca 4 atunci 5 sfdaca sffunc"
runLex mainParser "func x() ret x() sf func"

runLex mainParser "daca 5 atunci daca 2 atunci 3 sfdaca daca 3 atunci 3 sfdaca sfdaca"
runLex mainParser "daca 5 atunci daca 2 atunci 3 sfdaca sfdaca"
runLex mainParser "daca 5 atunci 3 sfdaca"

runLex mainParser "daca 5 atunci daca 2 atunci 3 sfdaca daca 3 atunci 3 sfdaca sfdaca"

runLex mainParser "func x() daca 5 atunci 3 sf daca ret 5 sf func"
runLex mainParser "func x() daca 5 atunci 3 sf daca sffunc daca 5 atunci 3 sfdaca"


runLex mainParser "func x() daca 5 atunci 3 sfdaca sffunc"

runLex mainParser "func x() daca 5 atunci 3 sfdaca ret 5 sffunc"

runLex mainParser "func x() ret func y() 1 sffunc sffunc"

runLex mainParser "a=func x() ret x() sf func"
runLex mainParser "func x() ret 3 sf func"
runLex mainParser "a= func x() ret x() sf func"


runLex mainParser "a=[2]"
runLex mainParser "a=1"
runLex mainParser "a()"

-- ??
runLex mainParser "scrie a[2]"
