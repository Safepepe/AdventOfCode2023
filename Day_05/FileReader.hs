module FileReader where
import Parsing

filename = "input.txt"

type Source = Int
type Destination = Int
type Range = Int
type Seed = Int
type ID = Int
type Mapping = ((String,String), [((Source, Destination), Range)])
type Almanac = ([Seed], [Mapping])

inputIO :: IO Almanac
inputIO = readFile filename >>= return.fst.head.parse almanac

almanac :: Parser Almanac
almanac = do
          sds <- seeds
          mpings <- some mapping
          return (sds, mpings)


seeds :: Parser [Seed]
seeds = do
        string "seeds: "
        sds <- some natural
        many $ token $ char '\n'
        return sds

mapping :: Parser Mapping
mapping = 
        do
        src <- some letter
        string "-to-"
        destination <- some letter
        string " map:\n"
        mapDescription <- some triplet
        many $ token $ char '\n'
        return $ ((src,destination), mapDescription)


triplet:: Parser ((Destination, Source), Range)
triplet = do
          destNum <- natural 
          srcNum <- natural
          rng <- natural 
          many $ token $ char '\n'
          return ((destNum, srcNum), rng)

          
