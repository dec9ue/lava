import Lava
import Lava.Arithmetic
import Lava.Captain
import Lava.Combinational
import Lava.ConstructiveAnalysis
import Lava.Eprover
import Lava.Error
import Lava.Fixit
import Lava.Generic
import Lava.HeerHugo
import Lava.IOBuffering
import Lava.Isc
import Lava.Generic
import Lava.LavaDir
import Lava.LavaRandom
import Lava.Limmat
import Lava.Modoc
import Lava.MyST
import Lava.Netlist
import Lava.Operators
import Lava.Patterns
import Lava.Property
import Lava.Ref
import Lava.Retime
import Lava.Satnik
import Lava.Satzoo
import Lava.Sequent
import Lava.Sequential
import Lava.SequentialCircuits
import Lava.SequentialConstructive
import Lava.Signal
-- import Lava.SignalTry
import Lava.Smv
import Lava.Stable
import Lava.Table
import Lava.Test
import Lava.Verification
import Lava.Vhdl
import Lava.Vis
import Lava.Zchaff

mux_neg (x,y) = mux(inv x,y)

list_contains []     v = low
list_contains (h: t) v = mux(equal( h , v), ((list_contains t v),high ))

gtei :: ([Signal Bool],[Signal Bool]) -> Signal Bool
gtei (a,b)         = mux(equal(comparei (a,b),int (-1)),(high,low))

comparei :: ([Signal Bool],[Signal Bool]) -> Signal Int
comparei ([], [])             = int 0
comparei ([], b )             = mux(list_contains b high, (int 0,int(-1)))
comparei (a , [])             = mux(list_contains a high, (int 0,int 1))
comparei (a:atail , b:btail ) = mux(equal(priv_res,int 0),(priv_res,res))
  where
    priv_res = comparei(atail,btail) 
    res      = mux(a, (mux(b,(int 0,int (-1))),mux (b, (int 1,int 0))))

subsi  ( v, n)    = undefined

divi   ( v, n)    = undefined

modexp ( v, e, n) = undefined


