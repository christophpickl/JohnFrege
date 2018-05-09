data Container a = Empty | Full a

instance (Show a) => Show (Container a) where
    show (Empty) = "Empty "
    show (Full a) = "Full " ++ (show a)

addIt :: Num a => Container a -> Container a -> Container a
addIt (Full x) (Full y) = Full (x + y)
addIt c Empty = c
addIt Empty c = c

main :: IO ()
main = do
  -- putStrLn $ show $ (Full "a")
  putStrLn $ show $ (addIt (Full 22) (Full 20))
