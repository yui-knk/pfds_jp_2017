-- 演習問題2.1

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x:xs) = [l] ++ (suffixes xs)

-- O(n)時間で生成できること
-- suffixesは`l@(x:xs)`なので、もとのlistの要素を
-- 1つずつ処理していくため。

-- O(n)空間で表現できること
-- n^2では?
