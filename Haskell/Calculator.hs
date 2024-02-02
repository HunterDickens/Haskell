import System.IO

main :: IO ()
main = do
    putStrLn "Enter the first number:"
    num1 <- getLine
    putStrLn "Enter the second number:"
    num2 <- getLine
    putStrLn "Enter the operation (+, -, *, /):"
    op <- getLine
    let result = calculate (read num1) (read num2) op
    putStrLn ("The result is: " ++ show result)

calculate :: Float -> Float -> String -> Float
calculate x y "+" = x + y
calculate x y "-" = x - y
calculate x y "*" = x * y
calculate x y "/" = x / y
calculate _ _ _   = error "Invalid operation"
