import Data.List
import Data.Char

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
		newAlpha = [x | x\<-alpha, x > index]
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

