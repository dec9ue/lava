import Lava
import Lava.Arithmetic
import Lava.Signal

-- to use in interpreter..

-- import Lava.Captain
import Lava.Combinational
-- import Lava.ConstructiveAnalysis
-- import Lava.Eprover
-- import Lava.Error
-- import Lava.Fixit
import Lava.Generic
-- import Lava.HeerHugo
-- import Lava.IOBuffering
-- import Lava.Isc
-- import Lava.LavaDir
-- import Lava.LavaRandom
-- import Lava.Limmat
-- import Lava.Modoc
-- import Lava.MyST
-- import Lava.Netlist
-- import Lava.Operators
import Lava.Patterns
-- import Lava.Property
import Lava.Ref
-- import Lava.Retime
-- import Lava.Satnik
-- import Lava.Satzoo
-- import Lava.Sequent
-- import Lava.Sequential
-- import Lava.SequentialCircuits
-- import Lava.SequentialConstructive
-- import Lava.SignalTry
-- import Lava.Smv
-- import Lava.Stable
-- import Lava.Table
-- import Lava.Test
-- import Lava.Verification
-- import Lava.Vhdl
-- import Lava.Vis
-- import Lava.Zchaff

bchoice (x,(y,z)) =  orl [ andl[x,y],andl[inv x,z]]
b2choice (x,((y1,y2),(z1,z2))) = (bchoice (x,(y1,z1)),bchoice(x,(y2,z2)))
blchoice (x,(y,z)) = zipWith (\x y -> orl [x,y]) (map (\w ->andl [x,w]) y) (map (\w ->andl [inv x,w]) z)

blchoice2 (x,(y:ytail,z:ztail)) = bchoice (x,(y,z)) : blchoice2 (x,(ytail,ztail))
blchoice2 (x,([],[])) = [] 
blchoice2 (x,([] ,z)) = let y = replicate (length z) low in blchoice2 (x,(y,z))
blchoice2 (x,([] ,y)) = let z = replicate (length y) low in blchoice2 (x,(y,z))

mux_neg (x,y) = mux(inv x,y)

list_contains []     v = low
list_contains (h: t) v = bchoice(equal( h , v),(high,(list_contains t v)))

gtei :: ([Signal Bool],[Signal Bool]) -> Signal Bool
gtei (a,b)         = bchoice(equal(comparei (a,b),lti),(low,high))

eqi = (high,low)
gti = (low,high)
lti = (low,low)

comparei :: ([Signal Bool],[Signal Bool]) -> (Signal Bool,Signal Bool)
comparei ([], [])             = eqi
comparei ([], b )             = b2choice(list_contains b high, (lti,eqi))
comparei (a , [])             = b2choice(list_contains a high, (gti,eqi))
comparei (a:atail , b:btail ) = b2choice(equal(priv_res,eqi),(res,priv_res))
  where
    priv_res = comparei(atail,btail) 
    res      = b2choice(a, ((b2choice(b,(eqi,gti))),b2choice (b, (lti,eqi))))

test_comparei_r = map (\(x,y) -> (simulate (\r->equal(comparei x,y)) high))  test_comparei
test_comparei = 
 [
   (([],[low]),eqi),
   (([low],[]),eqi),
   (([],[high]),lti),
   (([high],[]),gti),
   (([low],[high]),lti),
   (([high],[low]),gti),
   (([low],[low]),eqi),
   (([],[]),eqi)
 ]

-- comparei :: ([Signal Bool],[Signal Bool]) -> Signal Int
-- comparei ([], [])             = int 0
-- comparei ([], b )             = mux(list_contains b high, (int 0,int(-1)))
-- comparei (a , [])             = mux(list_contains a high, (int 0,int 1))
-- comparei (a:atail , b:btail ) = mux(equal(priv_res,int 0),(priv_res,res))
--   where
--     priv_res = comparei(atail,btail) 
--     res      = mux(a, (mux(b,(int 0,int (-1))),mux (b, (int 1,int 0))))

subsi  ( v, n)    = undefined

divi   ( v, n)    = undefined

modexp ( v, e, n) = undefined


