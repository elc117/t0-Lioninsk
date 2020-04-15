--Aluno: Pedro Leonel

isBin :: String -> Bool
isBin "" = True
isBin (x:xs)
    |x=='1' || x=='0' =isBin xs
    |otherwise =False

isBin2 :: String -> Bool
isBin2 str =length str == length(filter (\x->x=='1' || x=='0')str)   


auxBin2Dec::[Int]->Int->Int
auxBin2Dec bits (-1) = 0
auxBin2Dec (x:xs) numPot = (2^numPot)*x + auxBin2Dec xs (numPot-1) 

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec2 :: [Int] -> Int
bin2dec2 [] = undefined
bin2dec2 bits = sum(zipWith (*) bits pows)
    where pows = [2^y |y <- reverse[0..length(bits)-1]]

auxdec2bin :: Int -> [Int]
auxdec2bin 0 = []
auxdec2bin num =
  let a = (mod num 2: auxdec2bin(div num 2))
  in a


dec2bin :: Int -> [Int]
dec2bin a = reverse (auxdec2bin a)

isHex :: String -> Bool
isHex "" = True
isHex (x:xs) 
    |elem x "123456789abcdefABCDEF" == False = False
    |otherwise = isHex xs 