module ListUtils where
  change :: [a] -> Int -> a -> [a]
  change list index x = let (first, _:second) = splitAt index list
                        in first ++ x:second
