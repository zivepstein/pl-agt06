transpose :: [[a]] -> [[a]]
transpose [[]] = []
transpose l = (map head l') : (transpose (map tail l')) where
	l' = filter (not . null) l