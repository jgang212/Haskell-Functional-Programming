import Recursion
import Control.Exception

runTests =
  assert (intersperse [1,2,3,4,5] 500 == [500,1,500,2,500,3,500,4,500,5,500]) "basic intersperse"
  : assert (intersperse [] 500 == [500]) "empty intersperse"
  : assert (intersperse ["a","b","c"] "asdf" == ["asdf","a","asdf","b","asdf","c","asdf"]) "string intersperse"
  : assert (weave [1, 2, 3] [] == [1, 2, 3]) "weave empty 2nd"
  : assert (weave [] ["a","b","c"] == ["a","b","c"]) "weave empty 1st"
  : assert (weave [1,2,3,5,6] [100,101] == [1,100,2,101,3,5,6]) "weave run out 2nd"
  : assert (weave ["a","b","c"] ["jack","jill","mary"] == ["a","jack","b","jill","c","mary"]) "weave normal"
  : assert (weave [100,101] [1,2,3,5,6] == [100,1,101,2,3,5,6]) "weave run out 1st"
  : assert (pairSwap [(1,'a'),(2,'b')] == [('a',1),('b',2)]) "pairSwap normal"
  : assert (pairSwap [("asdf",'a')] == [('a',"asdf")]) "pairSwap one"
  : assert (reverse' ["a"] == ["a"]) "reverse one"
  : assert (reverse' [1,2,3,4,5] == [5,4,3,2,1]) "reverse normal"
  : assert (unzip' [(1, 'a'), (2, 'b'), (3, 'c')] == ([1,2,3],"abc")) "unzip normal"
  : assert (unzip' [(1, 'a')] == ([1],"a")) "unzip one"
  : assert (zip' [1,2,3] ['a','b','c'] == [(1,'a'),(2,'b'),(3,'c')]) "zip normal"
  : []

main = do print runTests