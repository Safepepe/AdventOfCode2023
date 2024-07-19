module Day where
import FileReader  --inputIO :: IO ([Instruction],Network)
import Control.Monad.Reader

--data Instruction = L | R 
--    deriving(Eq,Show)
--type Node = String
--type Paths = Instruction -> Node
--type Branch = (Node, Paths)
--type Network = [Branch]

-- Part One --
type Graph a = Reader ([Instruction],Network) a
--initialNode = "AAA"

step :: Instruction -> Node -> Graph Node
step i n0 = do
            n0brnch <- snd.head.filter ((==n0).fst).snd <$> ask
            return $ n0brnch i

--rewrite this function using the state monad
advanceUntil :: (Int,[Instruction]) -> (Node -> Bool) -> Node -> Graph Int
advanceUntil (k,[]) condition node0 = fst <$> ask >>= \ins -> advanceUntil (k,ins) condition node0
advanceUntil (k,(i:is)) condition node0
    | condition node0 = return k
    | otherwise       = step i node0 >>= \node1 -> advanceUntil (k+1,is) condition node1

--Testing function
testAction :: Show a => Graph a -> IO ()
testAction m = do
               (ins, network) <- inputIO
               print $ runReader m (ins, network)
               return ()

-- Part Two --
initialNodes :: IO [Node]
initialNodes = inputIO >>= return.filter ((=='A').last).map fst.snd 

endCondition :: Node -> Bool
endCondition = ((=='Z').last)

answer2 :: IO Int --[Int]
answer2 = do 
          initNodes <- initialNodes
          let manyAnswers =  runReader (mapM (advanceUntil (0,[]) endCondition) initNodes)
          endSteps <- manyAnswers <$> inputIO
          return $ foldr1 lcm endSteps
