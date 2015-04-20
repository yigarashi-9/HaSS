
main :: IO ()
main = do
  insts  <- (getLine >>= parseFile)
  result <- run insts
  print result
