distribute :: c -> Either a b -> Either (c, a) (c, b)
distribute t (Left x) = Left (t, x)
distribute t (Right y) = Right (t, y)

x = distribute 5 (Left "xyz")
y = distribute "abc" (Right 8)

fromEither x =
    case x of
      Left (a, b) -> (a, b)
      Right (a, b) -> (a, b)

main = do
          putStrLn $ show $ fromEither x
          putStrLn $ show $ fromEither y
