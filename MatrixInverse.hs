

type Matrix = [[Rational]]


-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f m1= map (map f)  m1

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f  m1 m2= zipWith (\row1 row2 -> map (uncurry f) (zip row1 row2)) m1 m2 

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes lst = [take (k-1) lst ++ drop k lst | k<-[1..(length lst)]]
--[|]
-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors matrix = [[[mat !!j| mat <-matrix2] | j<-[0..((matrixWidth  matrix)-1)]]|k<-[1..((matrixHeight matrix))],let matrix2 = removeRow k ((map removes matrix))]

removeRow :: Int -> [Matrix]-> [Matrix] 
removeRow n matrix =  uncurry (\x y -> x ++ tail y) (splitAt (n-1) matrix)

minorsForFirst :: Matrix -> [Matrix]
minorsForFirst matrix= let matrix1= removeRow 0 (map removes matrix) in [[mat !! j|mat<-matrix1]| j<-[0..((matrixWidth  matrix)-1)]]
-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = [[(-1)^(height+width)| width <-[1..w]] |height<-[1..h]]
        
determinant :: Matrix -> Rational
determinant [[]] = error "Empty matrix"
determinant [[x]] = x
determinant [[x1,y1],[x2,y2]] = (x1*y2)-(y1*x2)
determinant matrix1 = let mrow = zipWith (*) (head(signMatrix (matrixHeight matrix1) (matrixWidth matrix1))) (head (matrix1)) in sum (foldr (\(k,j) acc-> (k*determinant j):acc) [] (zip mrow (minorsForFirst matrix1)))
--[k*determinant j| (k,j)<- (zip mrow (minorsForFirst matrix1))] 
cofactors :: Matrix -> Matrix
cofactors m = transpose (zipMatrix (*) (signMatrix (matrixWidth m) (matrixHeight m)) (mapMatrix determinant (minors m))) 
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = mapMatrix (k*) 

inverse :: Matrix -> Matrix
inverse mat@([[x1,y1],[x2,y2]]) = scaleMatrix (1/determinant mat)([[y2,-(y1)],[-(x2),x1]])
inverse m 
    | (determinant m) /=0 = scaleMatrix (1/(determinant m)) (cofactors m)
    | otherwise = error "Non invertible"
