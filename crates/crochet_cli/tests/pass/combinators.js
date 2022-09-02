export const i = (x)=>x;
export const k = (x)=>(y)=>x;
export const s = (f)=>(g)=>(x)=>f(x)(g(x));
export const skk = s(k)(k);
