import System.IO
cargar = do
	handle <- openFile "cod_c.c" ReadMode
	contents <- hGetContents handle
	putStr contents
	hClose handle