  $ ../main.exe
  $$
  \frac{
  }{
    \vdash i32.const m : [] \to [i32]
  }
  \qquad
  \frac{
  }{
    \vdash i32.op : [i32 i32] \to [i32]
  }
  \qquad
  \frac{
  }{
    \vdash i32.op : [i32 i32] \to [i32]
  }
  \qquad
  \frac{
  }{
    \vdash nop : [] \to []
  }
  \qquad
  \frac{
    \vdash e1 : [t1] \to [t2]
  
  \qquad
    \vdash e2 : [t1] \to [t2]
  }{
    \vdash if e1 else e2 end : [t1] \to [t2]
  }
  $$
  For all integers m:
  We have:
    $i32.const m$ takes $[]$ and returns $[i32]$.
  
  For all binary operations op:
  We have:
    $i32.op$ takes $[i32 i32]$ and returns $[i32]$.
  
  For all binary relations op:
  We have:
    $i32.op$ takes $[i32 i32]$ and returns $[i32]$.
  
  We have:
    $nop$ takes $[]$ and returns $[]$.
  
  For all instructions e1:
  For all instructions e2:
  For all value types t1:
  For all value types t2:
  If:
    1. $e1$ takes $[t1]$ and returns $[t2]$, 
    2. $e2$ takes $[t1]$ and returns $[t2]$
  then:
    $if e1 else e2 end$ takes $[t1]$ and returns $[t2]$.
