-- 使用Haskell解决地图着色问题（请参考4.2节）
-- ch4/day1/map.pl
-- https://github.com/TimBastiaans/Haskell/blob/master/mapColoring.hs

module MapColoring where

colorStates = [ (alabama, mississippi, georgia, tennessee, florida) |
    alabama <- colors,
    mississippi <- colors,
    georgia <- colors,
    tennessee <- colors,
    florida <- colors,
    alabama /= tennessee,
    alabama /= mississippi,
    alabama /= georgia,
    alabama /= florida,
    georgia /= florida,
    georgia /= tennessee,
    mississippi /= tennessee ]
  where colors = [ "red", "green", "blue" ]

main = do
  print colorStates

-- [
--   ("red","green","green","blue","blue"),
--   ("red","blue","blue","green","green"),
--   ("green","red","red","blue","blue"),
--   ("green","blue","blue","red","red"),
--   ("blue","red","red","green","green"),
--   ("blue","green","green","red","red")
-- ]
