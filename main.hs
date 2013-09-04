import System.IO 
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.ByteString as Str

--Main
main = do
	putStr "\n~~~~~PROYECTO DE LENGUAJES~~~~~\n"
	putStr ""
	handle <- openFile "cod_c.c" ReadMode
	reser <-openFile "tok_c.txt" ReadMode
	contents <- hGetContents handle
	reservad<-hGetContents reser
	let conten=cropCommentsFromString contents 
	let reservadas=splitOn "," reservad
	let espacio=words conten
	escribir espacio reservadas
	hClose reser
	hClose handle
	
--Escribir escribe en el archivo una lista de tokens
escribir (x:xs) []=error "asdasd2"
escribir [] c=putStr "Finalizo!\n" 
escribir (x:xs) (c)
	| (x `elem` c)==True=do{appendFile "tok.txt" (x++",Palabra Reservada\n");escribir (xs) c}
	| (x =="(")==True=do{appendFile "tok.txt" (x++",Parentesis Abierto\n");escribir (xs) c}
	| (x ==")")==True=do{appendFile "tok.txt" (x++",Parentesis Cerrado\n");escribir (xs) c}
	| (x =="{")==True=do{appendFile "tok.txt" (x++",Llave Abierta\n");escribir (xs) c}
	| (x =="}")==True=do{appendFile "tok.txt" (x++",Llave Cerrada\n");escribir (xs) c}
	| (x ==";")==True=do{appendFile "tok.txt" (x++",Punto y coma\n");escribir (xs) c}
	| (x ==":")==True=do{appendFile "tok.txt" (x++",Punto y coma\n");escribir (xs) c}
	| (x ==",")==True=do{appendFile "tok.txt" (x++",Coma\n");escribir (xs) c}
	| (x ==">")==True=do{appendFile "tok.txt" (x++",Mayor\n");escribir (xs) c}
	| (x =="<")==True=do{appendFile "tok.txt" (x++",Menor\n");escribir (xs) c}
	| (x ==">=")==True=do{appendFile "tok.txt" (x++",Mayor Igual\n");escribir (xs) c}
	| (x =="<=")==True=do{appendFile "tok.txt" (x++",Menor Igual\n");escribir (xs) c}
	| (x =="=")==True=do{appendFile "tok.txt" (x++",Asignacion\n");escribir (xs) c}
	| (x =="==")==True=do{appendFile "tok.txt" (x++",Equivalencia\n");escribir (xs) c}
	| (x =="--")==True=do{appendFile "tok.txt" (x++",Disminucion Uitaria\n");escribir (xs) c}
	| (x =="++")==True=do{appendFile "tok.txt" (x++",Aumento Unitario\n");escribir (xs) c}
	| (x =="+")==True=do{appendFile "tok.txt" (x++",Suma\n");escribir (xs) c}
	| (x =="-")==True=do{appendFile "tok.txt" (x++",Resta\n");escribir (xs) c}
	| (x =="*")==True=do{appendFile "tok.txt" (x++",Multiplicacion\n");escribir (xs) c}
	| (head x =='*')==True=do{appendFile "tok.txt" ("*,Puntero\n");escribir ((tail x):xs) c}
	| (head x =='#')==True=do{appendFile "tok.txt" ("#,Numeral\n");escribir ((tail x):xs) c}
	| otherwise = do{appendFile "tok.txt" (x++",Identificador\n");escribir (xs) c}

isSubstringContainedInString :: String -> String -> Bool
isSubstringContainedInString substring string = isInfixOf substring string

cropParagraphCommentsFromString :: String -> String
cropParagraphCommentsFromString  string
	| ((isSubstringContainedInString "/*" string) && (isSubstringContainedInString "*/" string)) == False = string
cropParagraphCommentsFromString string = [string !! x | x<-[0..finalPoint - 1]++[initialPoint..(length string) - 1]]
	where
		finalPoint = (indexOfSubstringInString "/*" string)
		initialPoint = (indexOfSubstringInString "*/" string) + 2

cropSubstringFromString :: String -> String -> String
cropSubstringFromString substring string
	| (isSubstringContainedInString substring string) == False = ""
cropSubstringFromString substring string = [string !! x | x<-[0..finalPoint - 1]++[initialPoint..(length string) - 1]]
	where
		finalPoint = (indexOfSubstringInString substring string)
		initialPoint = (indexOfSubstringInString substring string) + (length substring)

displayNumber :: Integer -> Integer
displayNumber n = n

displayText :: String -> String
displayText string = string

indexOfSubstringInString :: String -> String -> Int
indexOfSubstringInString substring string
	| (null substring || null string) == True = length string
	| (isSubstringContainedInString substring string) == False = length string
indexOfSubstringInString substring string = indexOfSubstringInString' substring string ((length string) - (length substring))

indexOfSubstringInString' :: String -> String -> Int -> Int
indexOfSubstringInString' substring string index
	| (isSubstringContainedInString substring (init string)) == False = index
indexOfSubstringInString' substring string index = indexOfSubstringInString' substring (init string) (index - 1)


----


numberExistsInList :: Int -> [Int] -> Bool
numberExistsInList number list
	| (null list) == True = False
	| (head list) == number = True
numberExistsInList number list = numberExistsInList number (tail list)


--Alpha

alphaIndexesFinderInString :: String -> [Int]
alphaIndexesFinderInString string = [x | x<-[0..(length string) - 2], (((string !! x) == '/') && ((string !! (x + 1))) == '*')]

replaceAlphaCommentInStringInIndexes :: String -> String
replaceAlphaCommentInStringInIndexes string = [(replaceAlphaCommentInStringInIndexes' string x (alphaIndexesFinderInString string)) | x<-[0..(length string) - 1]]

replaceAlphaCommentInStringInIndexes' :: String -> Int -> [Int] -> Char
replaceAlphaCommentInStringInIndexes' string index indexes
	| (numberExistsInList index indexes) == True = '@'
	| (numberExistsInList (index - 1) indexes) == True = '?'
	| (numberExistsInList index indexes) == False = (string !! index)


--Omega

omegaIndexesFinderInString :: String -> [Int]
omegaIndexesFinderInString string = [x | x<-[0..(length string) - 2], (((string !! x) == '*') && ((string !! (x + 1))) == '/')]

replaceOmegaCommentInStringInIndexes :: String -> String
replaceOmegaCommentInStringInIndexes string = [(replaceOmegaCommentInStringInIndexes' string x (omegaIndexesFinderInString string)) | x<-[0..(length string) - 1]]

replaceOmegaCommentInStringInIndexes' :: String -> Int -> [Int] -> Char
replaceOmegaCommentInStringInIndexes' string index indexes
	| (numberExistsInList index indexes) == True = '$'
	| (numberExistsInList (index - 1) indexes) == True = '^'
	| (numberExistsInList index indexes) == False = (string !! index)


--Coment

replaceAllCommentSigns :: String -> String
replaceAllCommentSigns string = replaceOmegaCommentInStringInIndexes (replaceAlphaCommentInStringInIndexes string)

matchCommentSigns :: String -> [[Int]]
matchCommentSigns string = [[x, (matchCommentSigns' alpha omega x lengthx) + 1] | x<-alpha]
	where
		alpha = alphaIndexesFinderInString string
		omega = omegaIndexesFinderInString (replaceAlphaCommentInStringInIndexes string)
		lengthx = length string

matchCommentSigns' :: [Int] -> [Int] -> Int -> Int -> Int
matchCommentSigns' alpha omega index lengthx = matchCommentSigns'' newAlpha newOmega 0 lengthx
	where
		newAlpha = [x | x<-alpha, x > index]
		newOmega = [x | x<-omega, x > index]
	
matchCommentSigns'' :: [Int] -> [Int] -> Int -> Int -> Int
matchCommentSigns'' alpha omega counter lengthx
	| (null omega) == True = lengthx
matchCommentSigns'' alpha omega counter lengthx =
	if (null alpha) == False
		then if ((head alpha) < (head omega)) == True
			then (matchCommentSigns'' (tail alpha) omega (counter + 1) lengthx)
			else if ((head omega) < (head alpha))
				then if (counter /= 0)
					then (matchCommentSigns'' alpha (tail omega) (counter - 1) lengthx)
					else head omega
			else 0

		else if (null alpha) == True
			then if (counter /= 0)
				then (matchCommentSigns'' alpha (tail omega) (counter - 1) lengthx)
				else head omega
			else 0

cropCommentsFromString :: String -> String
cropCommentsFromString string = [(string !! x) | x<-[0..(length string) - 1], (numberInsideIndexes x indexes) == False]
	where
		indexes = matchCommentSigns string

numberInsideIndexes :: Int -> [[Int]] -> Bool
numberInsideIndexes index indexes
	| (null indexes) == True = False
	| ((index >= (head (head indexes))) && (index <= (last (head indexes)))) == True = True
numberInsideIndexes index indexes = numberInsideIndexes index (tail indexes)


cropSomething :: String -> [Int]
cropSomething s = slashIndexes
	where
		slashIndexes = [x | x<-[0..(length s) - 2], (s !! x) == '/' && (s !! (x + 1)) == '/']
		finalIndexes  = [x | x<-[0..(length (replaceSlashCommentInStringInIndexes s)) - 2], ((replaceSlashCommentInStringInIndexes s) !! x) == '/' && ((replaceSlashCommentInStringInIndexes s) !! (x + 1)) == 'n']

endIndexes :: [Int] -> [Int]
endIndexes y = [x | x<-[0..(length y) - 2], (getIndexInNumberList y x) /= ((getIndexInNumberList y (x + 1)) + 1)]

getIndexInNumberList :: [Int] -> Int -> Int
getIndexInNumberList x n = getIndexInNumberList' x 0
 
getIndexInNumberList' :: [Int] -> Int -> Int
getIndexInNumberList' x n
	| n == 0 = head(x)
getIndexInNumberList' x n = getIndexInNumberList' (tail x) (n - 1)
		
replaceSlashCommentInStringInIndexes :: String -> String
replaceSlashCommentInStringInIndexes string = [(replaceSlashCommentInStringInIndexes' string x (endIndexes [x | x<-[0..(length string) - 2], (string !! x) == '/' && (string !! (x + 1)) == '/'])) | x<-[0..(length string) - 2]]

replaceSlashCommentInStringInIndexes' :: String -> Int -> [Int] -> Char
replaceSlashCommentInStringInIndexes' string index indexes
	| (numberExistsInList index indexes) == True = '@'
	| (numberExistsInList (index - 1) indexes) == True = '@'
	| (numberExistsInList index indexes) == False = (string !! index)