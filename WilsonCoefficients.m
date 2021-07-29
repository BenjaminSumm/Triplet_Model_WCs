(* Created with the Wolfram Language : www.wolfram.com *)
(*In the following list the Wilson coefficients are expressed in terms 
of the original parameters with a tilde. Furthermore, all paramteres 
that exist both in the SM and in the triplet model are expressed through
SM parameters. k stands for a loop-factor of 1/(16*pi^2) and L = log(MV^2/Q^2),
where Q is the matching scale. Contributions from finite field strength renormalizations
have been explicitly added to the Wilson coefficients, so that all of the SMEFT fields 
are canonically normalized in the unbroken phase.*)

{CW -> -1/24*(g2^3*(1 - 2*gM^2 + 5*gM^4)*k*(2 + 3*gM^2*(-2 + L) + 3*L))/((-1 + gM^2)^2*MV^2), 

C\[Phi]W -> (g2^2*k*(-72*(1 - 10*gM^2 + 9*gM^4)*gVH + 
     gH^2*(-5 - 9*L + gM^4*(-11 + 3*L) + gM^2*(-56 + 54*L)) + 
     2*g2*gH*gM*(-41 - 9*L + gM^4*(-335 + 3*L) + gM^2*(304 + 54*L)) + 
     g2^2*gM^2*(-41 - 9*L + gM^4*(-335 + 3*L) + gM^2*(304 + 54*L))))/
   (96*(-1 + gM^2)^2*MV^2), 

C\[Phi]B -> (-5*g1^2*(gH + g2*gM)^2*k)/(64*MV^2), 

C\[Phi]WB -> (g1*g2*k*(2*gH^2*(8 - 6*L + gM^2*(-17 + 24*L)) + 
     4*g2*gH*gM*(17 - 3*L + gM^2*(-35 + 48*L)) + 
     g2^2*(-9*(1 + L) + gM^2*(94 + 54*L) + gM^4*(-157 + 93*L))))/
   (288*(-1 + gM^2)*MV^2), 

C\[Phi] -> -(gH+g2*gM)^2*lambda/(2*MV^2)+(k*(-432*(2*g2*gH*gM + g2^2*gM^2 + 2*gVH)^3 - 
     324*(gH + g2*gM)^4*(-1 + gM^2)*(-5 + 6*L)*lambda + 
     36*(gH + g2*gM)^2*(-1 + gM^2)*(2*g2*gH*gM + g2^2*gM^2 + 2*gVH)*
      (-29 + 24*L)*lambda - 18*(gH + g2*gM)*
      (16*g2^3*gM*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*lambda + 
       (gH + g2*gM)*(-2*g1^2*(gH + g2*gM)^2*(-1 + gM^2) - 
         6*g2^2*(gH + g2*gM)^2*(-1 + gM^2) - 
         60*(2*g2*gH*gM - g2^2*gM^2 + 2*gVH)^2 - 48*(gH + g2*gM)^2*
          (-1 + gM^2)*gVH*(-1 + 3*L) - 12*(gH + g2*gM)^2*(-1 + gM^2)*
          (-5 + 6*L)*lambda)) + (1 - gM^2)*lambda*
      (144*g2^4*(-1 + gM^2)^2*(1 + L) + 192*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 
       576*g2^2*gM^2*(gH + g2*gM)*(-(gH*L) + g2*gM*(4 + 11*L)) + 
       g2^2*(-192*g2^2*gM^4*(-5 + L) + 384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*
          (-1 + 6*L) + (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
       9*(gH + g2*gM)^2*(32*g2*gM*(gH + g2*gM)*(5 - 6*L) + 
         2*(gH + g2*gM)^2*(-5 + 6*L) + 3*g1^2*(-1 + gM^2)*(-5 + 6*L) - 
         96*g2^2*gM^2*(-1 + 6*L) - 4*(-1 + gM^2)*(-37 + 42*L)*lambda))))/
   (576*(-1 + gM^2)^2*MV^2),

C\[Phi]D -> (g1^2*(gH + g2*gM)^2*k*(-7 + 12*L))/
   (24*MV^2),

C\[Phi]Box -> -3/8*(gH+g2*gM)^2/MV^2+(k*(-216*(gH + g2*gM)^4*(-1 + gM^2)*(-5 + 6*L) - 288*g2^3*gM*(gH + g2*gM)*
      (gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L)) + 
     (1 - gM^2)*(144*g2^4*(-1 + gM^2)^2*(1 + L) + 192*g2^4*gM^2*(-1 + gM^2)*
        (2 + 3*L) + 2*g1^2*(gH + g2*gM)^2*(-1 + gM^2)*(-47 + 42*L) - 
       576*g2^2*gM^2*(gH + g2*gM)*(-(gH*L) + g2*gM*(4 + 11*L)) + 
       2*g2^2*(-96*g2^2*gM^4*(-5 + L) + 192*g2*gM*(gH + g2*gM)*(-1 + gM^2)*
          (-1 + 6*L) + (gH + g2*gM)^2*(-1 + gM^2)*(-137 + 78*L)) + 
       9*(32*g2*gM*(gH + g2*gM)^3*(5 - 6*L) - 
         96*(2*g2*gH*gM + g2^2*gM^2 + 2*gVH)^2*L + (gH + g2*gM)^4*
          (-5 + 6*L) + 8*(gH + g2*gM)^2*(2*g2^2*gM^2*(10 - 39*L) - 
           4*g2*gH*gM*(-4 + 3*L) - 4*gVH*(-4 + 3*L) - 3*(-1 + gM^2)*
            (-5 + 6*L)*lambda)))))/(768*(-1 + gM^2)^2*MV^2), 

Ce\[Phi][flav1_, flav2_] :> -(gH+g2*gM)*ye[flav1,flav2]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(gl + g2*gM)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*ye[flav1, flav2] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      ye[flav1, flav2] + (1 - gM^2)*((144*g2^4*(-1 + gM^2)^2*(1 + L) + 
         192*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*
          (-(gH*L) + g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(gl + g2*gM)*(-1 + gM^2)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        ye[flav1, flav2] + 144*(gl + g2*gM)^2*(-1 + gM^2)*ye[flav1, flav10]*
        ye[flav11, flav2]*conj[ye][flav11, flav10])))/
   (1152*(-1 + gM^2)^2*MV^2),

conj[Ce\[Phi]][flav2_, flav1_] :> -(gH+g2*gM)*conj[ye][flav2,flav1]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(gl + g2*gM)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*conj[ye][flav2, flav1] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      conj[ye][flav2, flav1] + (1 - gM^2)*
      ((144*g2^4*(-1 + gM^2)^2*(1 + L) + 192*g2^4*gM^2*(-1 + gM^2)*
          (2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*(-(gH*L) + 
           g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(gl + g2*gM)*(-1 + gM^2)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        conj[ye][flav2, flav1] + 144*(gl + g2*gM)^2*(-1 + gM^2)*
        ye[flav12, flav13]*conj[ye][flav12, flav1]*conj[ye][flav2, flav13])))/
   (1152*(-1 + gM^2)^2*MV^2), 

Cd\[Phi][flav1_, flav2_] :> -(gH+g2*gM)*yd[flav1,flav2]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(g2*gM + gq)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*yd[flav1, flav2] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      yd[flav1, flav2] + (1 - gM^2)*((144*g2^4*(-1 + gM^2)^2*(1 + L) + 
         192*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*
          (-(gH*L) + g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(-1 + gM^2)*(g2*gM + gq)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        yd[flav1, flav2] + 144*(-1 + gM^2)*(g2*gM + gq)^2*yd[flav1, flav14]*
        yd[flav15, flav2]*conj[yd][flav15, flav14] - 
       72*(-1 + gM^2)*(g2*gM + gq)^2*(-11 + 6*L)*yd[flav16, flav2]*
        yu[flav1, flav17]*conj[yu][flav16, flav17])))/
   (1152*(-1 + gM^2)^2*MV^2), 

conj[Cd\[Phi]][flav2_, flav1_] :> -(gH+g2*gM)*conj[yd][flav2,flav1]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(g2*gM + gq)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*conj[yd][flav2, flav1] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      conj[yd][flav2, flav1] + (1 - gM^2)*
      ((144*g2^4*(-1 + gM^2)^2*(1 + L) + 192*g2^4*gM^2*(-1 + gM^2)*
          (2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*(-(gH*L) + 
           g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(-1 + gM^2)*(g2*gM + gq)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        conj[yd][flav2, flav1] + 144*(-1 + gM^2)*(g2*gM + gq)^2*
        yd[flav18, flav19]*conj[yd][flav2, flav19]*conj[yd][flav18, flav1] - 
       72*(-1 + gM^2)*(g2*gM + gq)^2*(-11 + 6*L)*yu[flav20, flav21]*
        conj[yd][flav20, flav1]*conj[yu][flav2, flav21])))/
   (1152*(-1 + gM^2)^2*MV^2), 

Cu\[Phi][flav1_, flav2_] :> -(gH+g2*gM)*yu[flav1,flav2]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(g2*gM + gq)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*yu[flav1, flav2] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      yu[flav1, flav2] + (1 - gM^2)*((144*g2^4*(-1 + gM^2)^2*(1 + L) + 
         192*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*
          (-(gH*L) + g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(-1 + gM^2)*(g2*gM + gq)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        yu[flav1, flav2] - 72*(-1 + gM^2)*(g2*gM + gq)^2*(-11 + 6*L)*
        yd[flav1, flav25]*yu[flav22, flav2]*conj[yd][flav22, flav25] + 
       144*(-1 + gM^2)*(g2*gM + gq)^2*yu[flav1, flav23]*yu[flav24, flav2]*
        conj[yu][flav24, flav23])))/(1152*(-1 + gM^2)^2*MV^2), 


conj[Cu\[Phi]][flav2_, flav1_] :> -(gH+g2*gM)*ye[flav2,flav1]/(4*MV^2)+(k*(-54*(gH + g2*gM)^2*(1 - gM^2)*(3*(g2*gM + gq)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*conj[yu][flav2, flav1] - 
     288*g2^3*gM*(gH + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      conj[yu][flav2, flav1] + (1 - gM^2)*
      ((144*g2^4*(-1 + gM^2)^2*(1 + L) + 192*g2^4*gM^2*(-1 + gM^2)*
          (2 + 3*L) - 576*g2^2*gM^2*(gH + g2*gM)*(-(gH*L) + 
           g2*gM*(4 + 11*L)) + g2^2*(-192*g2^2*gM^4*(-5 + L) + 
           384*g2*gM*(gH + g2*gM)*(-1 + gM^2)*(-1 + 6*L) + 
           (gH + g2*gM)^2*(-1 + gM^2)*(-229 + 102*L)) + 
         9*(gH + g2*gM)*(32*g2*gM*(gH + g2*gM)^2*(5 - 6*L) + 
           2*(gH + g2*gM)^3*(-5 + 6*L) + 3*g1^2*(gH + g2*gM)*(-1 + gM^2)*
            (-5 + 6*L) - 24*(-1 + gM^2)*(g2*gM + gq)*(-1 + 2*L)*lambda + 
           2*(gH + g2*gM)*(2*g2^2*gM^2*(53 - 168*L) + gVH*(116 - 96*L) + 
             4*g2*gH*gM*(29 - 24*L) - (-1 + gM^2)*(-73 + 78*L)*lambda)))*
        conj[yu][flav2, flav1] - 72*(-1 + gM^2)*(g2*gM + gq)^2*(-11 + 6*L)*
        yd[flav26, flav29]*conj[yd][flav2, flav29]*conj[yu][flav26, flav1] + 
       144*(-1 + gM^2)*(g2*gM + gq)^2*yu[flav27, flav28]*
        conj[yu][flav2, flav28]*conj[yu][flav27, flav1])))/
   (1152*(-1 + gM^2)^2*MV^2), 

CeW[flav1_, flav2_] :> -1/576*(g2*(gl + g2*gM)*k*(gl*(85 + gM^2*(131 - 132*L) - 12*L) + 
      g2*gM*(-1 + gM^2)*(-247 + 84*L) + 18*gH*(9 - 4*L + 3*gM^2*(-7 + 4*L)))*
     ye[flav1, flav2])/((-1 + gM^2)*MV^2), 

conj[CeW][flav2_, flav1_] :> -1/576*(g2*(gl + g2*gM)*k*(gl*(85 + gM^2*(131 - 132*L) - 12*L) + 
      g2*gM*(-1 + gM^2)*(-247 + 84*L) + 18*gH*(9 - 4*L + 3*gM^2*(-7 + 4*L)))*
     conj[ye][flav2, flav1])/((-1 + gM^2)*MV^2), 

CeB[flav1_, flav2_] :> -1/32*(g1*(gl + g2*gM)*(-3*gH + 4*gl + g2*gM)*k*
     ye[flav1, flav2])/MV^2, 

conj[CeB][flav2_, flav1_] :> -1/32*(g1*(gl + g2*gM)*(-3*gH + 4*gl + g2*gM)*k*conj[ye][flav2, flav1])/
    MV^2, 

CdW[flav1_, flav2_] :> -1/576*(g2*(g2*gM + gq)*k*(gq*(85 + gM^2*(131 - 132*L) - 12*L) + 
      g2*gM*(-1 + gM^2)*(-247 + 84*L) + 18*gH*(9 - 4*L + 3*gM^2*(-7 + 4*L)))*
     yd[flav1, flav2])/((-1 + gM^2)*MV^2), conj[CdW][flav2_, flav1_] :> 
  -1/576*(g2*(g2*gM + gq)*k*(gq*(85 + gM^2*(131 - 132*L) - 12*L) + 
      g2*gM*(-1 + gM^2)*(-247 + 84*L) + 18*gH*(9 - 4*L + 3*gM^2*(-7 + 4*L)))*
     conj[yd][flav2, flav1])/((-1 + gM^2)*MV^2), 

CdB[flav1_, flav2_] :> (g1*(g2*gM + gq)*(9*gH + 13*g2*gM + 4*gq)*k*
    yd[flav1, flav2])/(96*MV^2), 

conj[CdB][flav2_, flav1_] :> (g1*(g2*gM + gq)*(9*gH + 13*g2*gM + 4*gq)*k*conj[yd][flav2, flav1])/
   (96*MV^2), 

CuW[flav1_, flav2_] :> (g2*(g2*gM + gq)*k*(18*gH*(-9 + 4*L + gM^2*(-3 + 4*L)) + 
     gq*(-85 + 12*L + gM^2*(-131 + 132*L)) + 
     g2*gM*(-247 + 84*L + gM^2*(-185 + 204*L)))*yu[flav1, flav2])/
   (576*(-1 + gM^2)*MV^2), 

conj[CuW][flav2_, flav1_] :> (g2*(g2*gM + gq)*k*(18*gH*(-9 + 4*L + gM^2*(-3 + 4*L)) + 
     gq*(-85 + 12*L + gM^2*(-131 + 132*L)) + 
     g2*gM*(-247 + 84*L + gM^2*(-185 + 204*L)))*conj[yu][flav2, flav1])/
   (576*(-1 + gM^2)*MV^2), 

CuB[flav1_, flav2_] :> -1/96*(g1*(9*gH + 5*g2*gM - 4*gq)*(g2*gM + gq)*k*yu[flav1, flav2])/MV^2, 
 
conj[CuB][flav2_, flav1_] :> -1/96*(g1*(9*gH + 5*g2*gM - 4*gq)*(g2*gM + gq)*k*conj[yu][flav2, flav1])/
    MV^2, 

C\[Phi]l1[flav1_, flav2_] :> 
  -1/768*(k*(g1^2*(11*g2^2*gM^2*(7 + 6*L) + gH^2*(19 + 6*L) + 
        2*g2*gH*gM*(19 + 6*L) + 4*g2*gl*gM*(29 + 30*L) + gl^2*(58 + 60*L))*
       delta[flav1, flav2] + 36*(gl + g2*gM)*(gl + gH*(7 - 10*L) - 
        4*g2*gM*(-2 + L) + 6*gl*L)*ye[flav1, flav30]*
       conj[ye][flav2, flav30]))/MV^2, 

C\[Phi]l3[flav1_, flav2_] :> -(gl+g2*gM)*(gH+g2*gM)*delta[flav1,flav2]/(4*MV^2)+(k*(72*(gH + g2*gM)*(gl + g2*gM)*(1 - gM^2)*(9*(gl + g2*gM)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*delta[flav1, flav2] - 
     288*g2^3*gM*(gH + gl + 2*g2*gM)*(gM^2*(4 - 18*L) + 3*L + 
       gM^4*(4 + 15*L))*delta[flav1, flav2] + 
     (1 - gM^2)*((g2^4*(gM^4*(2261 - 18534*L) + gM^2*(1531 - 8490*L) + 
           288*(1 + L)) + 72*gH*gl*(-3*gl^2 + 4*gVH*(5 - 6*L) + 
           gH*gl*(-5 + 6*L)) - 72*g2*gM*(3*gl^3 + 6*gH^2*gl*(-5 + 6*L) + 
           4*gl*gVH*(-5 + 6*L) - gH*(4*gVH*(5 - 6*L) + gl^2*(5 + 12*L))) - 
         2*g2^3*gM*(18*gl*(-65 + 118*L + gM^2*(-93 + 394*L)) + 
           gH*(-1321 + 2334*L + gM^2*(-2027 + 9186*L))) + 
         g2^2*(-72*gH*gl*(6*(-5 + 6*L) + gM^2*(-103 + 192*L)) + 
           gH^2*(49 + 114*L - 7*gM^2*(-353 + 366*L)) + 
           6*(48*gM^2*gVH*(5 - 6*L) + gl^2*(-17 + 54*L + 5*gM^2*
                (37 + 42*L)))))*delta[flav1, flav2] - 
       36*(gl + g2*gM)*(-1 + gM^2)*(gl + 6*gl*L + 12*g2*gM*L + gH*(-1 + 6*L))*
        ye[flav1, flav31]*conj[ye][flav2, flav31])))/(2304*(-1 + gM^2)^2*MV^2), 

C\[Phi]q1[flav1_, flav2_] :> (k*(g1^2*((gH + g2*gM)^2*(19 + 6*L) + 2*(g2*gM + gq)^2*(29 + 30*L))*
      delta[flav1, flav2] - 108*(g2*gM + gq)*(gq + gH*(7 - 10*L) - 
       4*g2*gM*(-2 + L) + 6*gq*L)*(yd[flav1, flav32]*conj[yd][flav2, 
         flav32] - yu[flav1, flav33]*conj[yu][flav2, flav33])))/(2304*MV^2), 


C\[Phi]q3[flav1_, flav2_] :> -(gq+g2*gM)*(gH+g2*gM)*delta[flav1,flav2]/(4*MV^2)+(k*(72*(gH + g2*gM)*(gl + g2*gM)*(1 - gM^2)*(9*(gl + g2*gM)^2 - 
       3*(gH + g2*gM)^2*(-5 + 6*L))*delta[flav1, flav2] - 
     288*g2^3*gM*(gH + 2*g2*gM + gq)*(gM^2*(4 - 18*L) + 3*L + 
       gM^4*(4 + 15*L))*delta[flav1, flav2] + 
     (1 - gM^2)*((g2^4*(gM^4*(2261 - 18534*L) + gM^2*(1531 - 8490*L) + 
           288*(1 + L)) + 72*gH*gq*(-3*gq^2 + 4*gVH*(5 - 6*L) + 
           gH*gq*(-5 + 6*L)) - 72*g2*gM*(3*gq^3 + 6*gH^2*gq*(-5 + 6*L) + 
           4*gq*gVH*(-5 + 6*L) - gH*(4*gVH*(5 - 6*L) + gq^2*(5 + 12*L))) - 
         2*g2^3*gM*(18*gq*(-65 + 118*L + gM^2*(-93 + 394*L)) + 
           gH*(-1321 + 2334*L + gM^2*(-2027 + 9186*L))) + 
         g2^2*(-72*gH*gq*(6*(-5 + 6*L) + gM^2*(-103 + 192*L)) + 
           gH^2*(49 + 114*L - 7*gM^2*(-353 + 366*L)) + 
           6*(48*gM^2*gVH*(5 - 6*L) + gq^2*(-17 + 54*L + 5*gM^2*
                (37 + 42*L)))))*delta[flav1, flav2] - 
       36*(-1 + gM^2)*(g2*gM + gq)*(gq + 12*g2*gM*L + 6*gq*L + gH*(-1 + 6*L))*
        (yd[flav1, flav34]*conj[yd][flav2, flav34] + yu[flav1, flav35]*
          conj[yu][flav2, flav35]))))/(2304*(-1 + gM^2)^2*MV^2), 

C\[Phi]e[flav1_, flav2_] :> -1/384*(k*(g1^2*(gH + g2*gM)^2*(19 + 6*L)*delta[flav1, flav2] + 
      12*(gl + g2*gM)*(2*g2*gM + gH*(9 - 6*L) + gl*(-7 + 6*L))*
       ye[flav3, flav2]*conj[ye][flav3, flav1]))/MV^2, 


C\[Phi]u[flav1_, flav2_] :> (k*(g1^2*(gH + g2*gM)^2*(19 + 6*L)*delta[flav1, flav2] + 
     18*(g2*gM + gq)*(2*g2*gM + gH*(9 - 6*L) + gq*(-7 + 6*L))*
      yu[flav3, flav2]*conj[yu][flav3, flav1]))/(576*MV^2), 


C\[Phi]d[flav1_, flav2_] :> -1/1152*(k*(g1^2*(gH + g2*gM)^2*(19 + 6*L)*delta[flav1, flav2] + 
      36*(g2*gM + gq)*(2*g2*gM + gH*(9 - 6*L) + gq*(-7 + 6*L))*
       yd[flav3, flav2]*conj[yd][flav3, flav1]))/MV^2, 


C\[Phi]ud[flav1_, flav2_] :> -1/16*((g2*gM + gq)*k*(2*g2*gM + gH*(9 - 6*L) + gq*(-7 + 6*L))*
     yd[flav3, flav2]*conj[yu][flav3, flav1])/MV^2, 


conj[C\[Phi]ud][flav2_, flav1_] :> -1/16*((g2*gM + gq)*k*(2*g2*gM + gH*(9 - 6*L) + gq*(-7 + 6*L))*
     yu[flav3, flav2]*conj[yd][flav3, flav1])/MV^2, 

Cledq[flav1_, flav2_, flav3_, flav4_] :> -1/32*(k*(g2^2*gM^2*(-17 + 6*L) + 2*g2*gM*(gl + gq)*(-11 + 6*L) + 
      4*gl*gq*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
      2*gH*(-5*g2*gM - 3*gq + 6*g2*gM*L + 6*gq*L + gl*(-3 + 6*L)))*
     ye[flav1, flav2]*conj[yd][flav4, flav3])/MV^2, 

conj[Cledq][flav1_, flav2_, flav3_, flav4_] :> -1/32*(k*(g2^2*gM^2*(-17 + 6*L) + 2*g2*gM*(gl + gq)*(-11 + 6*L) + 
      4*gl*gq*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
      2*gH*(-5*g2*gM - 3*gq + 6*g2*gM*L + 6*gq*L + gl*(-3 + 6*L)))*
     yd[flav3, flav4]*conj[ye][flav2, flav1])/MV^2, 

Cquqd1[flav1_, flav2_, flav3_, flav4_] :> (k*((gH^2*(1 - 6*L) + 4*gq^2*(-11 + 6*L) + 4*g2*gM*gq*(-25 + 18*L) + 
       g2^2*gM^2*(-55 + 42*L) + 2*gH*(6*gq*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*
      yd[flav3, flav4]*yu[flav1, flav2] - 8*(g2*gM + gq)^2*(-3 + 2*L)*
      yd[flav1, flav4]*yu[flav3, flav2]))/(32*MV^2), 

conj[Cquqd1][flav1_, flav2_, flav3_, flav4_] :> (k*(((gH + g2*gM)^2*(1 - 6*L) + 12*(gH + g2*gM)*(g2*gM + gq)*(-1 + 2*L) + 
       4*(g2*gM + gq)^2*(-11 + 6*L))*conj[yd][flav4, flav3]*
      conj[yu][flav2, flav1] + 8*(g2*gM + gq)^2*(3 - 2*L)*
      conj[yd][flav2, flav3]*conj[yu][flav4, flav1]))/(32*MV^2), 

Cqd8[flav1_, flav2_, flav3_, flav4_] :> (k*(g3^2*(g2*gM + gq)^2*(29 + 30*L)*delta[flav1, flav2]*
      delta[flav3, flav4] + 3*(g2^2*gM^2*(-17 + 6*L) + 
       4*g2*gM*gq*(-11 + 6*L) + 4*gq^2*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
       2*gH*(6*gq*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*yd[flav1, flav4]*
      conj[yd][flav2, flav3]))/(96*MV^2),

Cqd1[flav1_, flav2_, flav3_, flav4_] :> (k*(-(g1^2*(g2*gM + gq)^2*(29 + 30*L)*delta[flav1, flav2]*
       delta[flav3, flav4]) + 9*(g2^2*gM^2*(-17 + 6*L) + 
       4*g2*gM*gq*(-11 + 6*L) + 4*gq^2*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
       2*gH*(6*gq*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*yd[flav1, flav4]*
      conj[yd][flav2, flav3]))/(1728*MV^2), 


Cle[flav1_, flav2_, flav3_, flav4_] :> (k*(g1^2*(gl + g2*gM)^2*(29 + 30*L)*delta[flav1, flav2]*
      delta[flav3, flav4] + 3*(g2^2*gM^2*(-17 + 6*L) + 
       4*g2*gl*gM*(-11 + 6*L) + 4*gl^2*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
       2*gH*(6*gl*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*ye[flav1, flav4]*
      conj[ye][flav2, flav3]))/(192*MV^2), 

Clequ1[flav1_, flav2_, flav3_, flav4_] :> (k*(8*gl*gq + g2^2*gM^2*(19 - 18*L) - 2*g2*gM*(gl + gq)*(-7 + 6*L) + 
     gH^2*(-1 + 6*L) - 2*gH*(-5*g2*gM - 3*gq + 6*g2*gM*L + 6*gq*L + 
       gl*(-3 + 6*L)))*ye[flav1, flav2]*yu[flav3, flav4])/(32*MV^2), 


conj[Clequ1][flav1_, flav2_, flav3_, flav4_] :> (k*(8*gl*gq + g2^2*gM^2*(19 - 18*L) - 2*g2*gM*(gl + gq)*(-7 + 6*L) + 
     gH^2*(-1 + 6*L) - 2*gH*(-5*g2*gM - 3*gq + 6*g2*gM*L + 6*gq*L + 
       gl*(-3 + 6*L)))*conj[ye][flav2, flav1]*conj[yu][flav4, flav3])/
   (32*MV^2), 

Cqu8[flav1_, flav2_, flav3_, flav4_] :> (k*(g3^2*(g2*gM + gq)^2*(29 + 30*L)*delta[flav1, flav2]*
      delta[flav3, flav4] + 3*(g2^2*gM^2*(-17 + 6*L) + 
       4*g2*gM*gq*(-11 + 6*L) + 4*gq^2*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
       2*gH*(6*gq*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*yu[flav1, flav4]*
      conj[yu][flav2, flav3]))/(96*MV^2), 

 Cqu1[flav1_, flav2_, flav3_, flav4_] :> (k*(2*g1^2*(g2*gM + gq)^2*(29 + 30*L)*delta[flav1, flav2]*
      delta[flav3, flav4] + 9*(g2^2*gM^2*(-17 + 6*L) + 
       4*g2*gM*gq*(-11 + 6*L) + 4*gq^2*(-7 + 6*L) + gH^2*(-1 + 6*L) - 
       2*gH*(6*gq*(-1 + 2*L) + g2*gM*(-5 + 6*L)))*yu[flav1, flav4]*
      conj[yu][flav2, flav3]))/(1728*MV^2), 

 Clu[flav1_, flav2_, flav3_, flav4_] :> -1/288*(g1^2*(gl + g2*gM)^2*k*(29 + 30*L)*delta[flav1, flav2]*
     delta[flav3, flav4])/MV^2,

Cld[flav1_, flav2_, flav3_, flav4_] :> (g1^2*(gl + g2*gM)^2*k*(29 + 30*L)*delta[flav1, flav2]*delta[flav3, flav4])/
   (576*MV^2), 

Cqe[flav1_, flav2_, flav3_, flav4_] :> -1/576*(g1^2*(g2*gM + gq)^2*k*(29 + 30*L)*delta[flav1, flav2]*
     delta[flav3, flav4])/MV^2, 

Cll[flav1_, flav2_, flav3_, flav4_] :>(gl+g2*gM)^2*(delta[flav1,flav2]*delta[flav3,flav4]-2*delta[flav1,flav4]*delta[flav2,flav3])/(8*MV^2)+(k*(108*(gl + g2*gM)^4*(-1 + gM^2)*(2*delta[flav1, flav4]*
        delta[flav2, flav3] - delta[flav1, flav2]*delta[flav3, flav4]) - 
     48*g2^3*gM*(gl + g2*gM)*(gM^2*(4 - 18*L) + 3*L + gM^4*(4 + 15*L))*
      (2*delta[flav1, flav4]*delta[flav2, flav3] - delta[flav1, flav2]*
        delta[flav3, flav4]) + (1 - gM^2)*
      (2*(-32*g2^4*gM^4*(-5 + L) + 24*g2^4*(-1 + gM^2)^2*(1 + L) + 
         32*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 96*g2^2*gM^2*(gl + g2*gM)*
          (gl*(-3 + L) + g2*gM*(1 + 13*L)) + 2*(gl + g2*gM)^2*
          (11*g2^2*gM^2*(13 - 36*L) - 18*g1^2*(-1 + gM^2)*(-3 + 2*L) + 
           gl^2*(-73 + 36*L) + 2*g2*gl*gM*(-1 + 36*L)) + 
         g2^2*(gl + g2*gM)*(-1 + gM^2)*(gl*(-471 + 378*L) + 
           g2*gM*(-535 + 762*L)))*delta[flav1, flav4]*delta[flav2, flav3] - 
       (-32*g2^4*gM^4*(-5 + L) + 24*g2^4*(-1 + gM^2)^2*(1 + L) + 
         32*g2^4*gM^2*(-1 + gM^2)*(2 + 3*L) - 96*g2^2*gM^2*(gl + g2*gM)*
          (gl*(-3 + L) + g2*gM*(1 + 13*L)) + (gl + g2*gM)^2*
          (2*g2^2*gM^2*(89 - 396*L) + 2*gl^2*(-127 + 36*L) + 
           4*g2*gl*gM*(-55 + 36*L) - g1^2*(-1 + gM^2)*(-137 + 42*L)) + 
         g2^2*(gl + g2*gM)*(-1 + gM^2)*(gl*(-795 + 594*L) + 
           g2*gM*(-859 + 978*L)))*delta[flav1, flav2]*delta[flav3, flav4])))/
   (384*(-1 + gM^2)^2*MV^2), 

Cqq1[flav1_, flav2_, flav3_, flav4_] :> ((g2*gM + gq)^2*k*(9*g3^2*(-79 + 102*L)*delta[flav1, flav4]*
      delta[flav2, flav3] + (474*g3^2 - (972*(g2*gM + gq)^2)/(-1 + gM^2) - 
       612*g3^2*L + 972*g2^2*(-3 + 2*L) + g1^2*(29 + 30*L))*
      delta[flav1, flav2]*delta[flav3, flav4]))/(3456*MV^2), 

Cqq3[flav1_, flav2_, flav3_, flav4_] :> -(gq+g2*gM)^2*delta[flav1,flav2]*delta[flav3,flav4]/(8*MV^2)-1/384*(k*(-108*(-1 + gM^2)*(g2*gM + gq)^4*delta[flav1, flav2]*
       delta[flav3, flav4] + 48*g2^3*gM*(g2*gM + gq)*(gM^2*(4 - 18*L) + 3*L + 
        gM^4*(4 + 15*L))*delta[flav1, flav2]*delta[flav3, flav4] + 
      (1 - gM^2)*(g3^2*(-1 + gM^2)*(g2*gM + gq)^2*(-79 + 102*L)*
         delta[flav1, flav4]*delta[flav2, flav3] - 
        (-4*g1^2*(-1 + gM^2)*gq^2*(-3 + 2*L) + 2*gq^4*(-73 + 36*L) - 
          8*g2*gM*gq*(gq^2*(37 - 36*L) + g1^2*(-1 + gM^2)*(-3 + 2*L)) - 
          2*g2^3*gM*gq*(-503 + 570*L + 3*gM^2*(41 + 274*L)) + 
          g2^4*(gM^2*(423 - 906*L) + 24*(1 + L) - gM^4*(97 + 1190*L)) - 
          g2^2*(4*g1^2*gM^2*(-1 + gM^2)*(-3 + 2*L) + 
            3*gq^2*(-157 + 126*L + gM^2*(17 + 50*L))))*delta[flav1, flav2]*
         delta[flav3, flav4])))/((-1 + gM^2)^2*MV^2), 

Clq1[flav1_, flav2_, flav3_, flav4_] :> 
  -1/1152*((1 - gM^2)*k*(g1^2*(1 - gM^2)*((gl + g2*gM)^2 + (g2*gM + gq)^2)*
       (29 + 30*L) - 648*(gl + g2*gM)*(g2*gM + gq)*(gl*gq + g2*gM*(gl + gq) + 
        g2^2*(-3 - 2*gM^2*(-2 + L) + 2*L)))*delta[flav1, flav2]*
     delta[flav3, flav4])/((-1 + gM^2)^2*MV^2), 

Clq3[flav1_, flav2_, flav3_, flav4_] :> -(gq+g2*gM)*(gl+g2*gM)*delta[flav1,flav2]*delta[flav3,flav4]/(4*MV^2)-1/384*(k*(2*g2^3*gM*(gl + gq)*(-503 + gM^2*(722 - 180*L) + 
        3*gM^4*(-9 - 154*L) + 642*L) + 4*g2*gM*(-1 + gM^2)*(gl + gq)*
       (-9*gl^2 - 9*gq^2 + 6*g1^2*(-1 + gM^2)*(-3 + 2*L) + 
        8*gl*gq*(-7 + 9*L)) + 4*gl*(-1 + gM^2)*gq*(-9*gl^2 - 9*gq^2 + 
        6*g1^2*(-1 + gM^2)*(-3 + 2*L) + gl*gq*(-55 + 36*L)) + 
      2*g2^4*(gM^4*(712 - 580*L) + gM^6*(95 - 470*L) - 24*(1 + L) + 
        3*gM^2*(-133 + 358*L)) + g2^2*(-1 + gM^2)*
       (24*g1^2*gM^2*(-1 + gM^2)*(-3 + 2*L) - 8*gl*gq*(2*(-61 + 54*L) + 
          gM^2*(19 + 108*L)) + gl^2*(-17 + 54*L + gM^2*(25 + 282*L)) + 
        gq^2*(-17 + 54*L + gM^2*(25 + 282*L))))*delta[flav1, flav2]*
     delta[flav3, flav4])/((-1 + gM^2)^2*MV^2), 

CdG[flav1_, flav2_] :> (g3*(g2*gM + gq)^2*k*yd[flav1, flav2])/(4*MV^2), 

conj[CdG][flav2_, flav1_] :> (g3*(g2*gM + gq)^2*k*conj[yd][flav2, flav1])/(4*MV^2),

CuG[flav1_, flav2_] :> (g3*(g2*gM + gq)^2*k*yu[flav1, flav2])/(4*MV^2), 

conj[CuG][flav2_, flav1_] :> (g3*(g2*gM + gq)^2*k*conj[yu][flav2, flav1])/(4*MV^2), 

Clequ3[flav1_, flav2_, flav3_, flav4_] :> (3*(gl + g2*gM)*(g2*gM + gq)*k*(-3 + 2*L)*ye[flav1, flav2]*
    yu[flav3, flav4])/(32*MV^2), 

conj[Clequ3][flav1_, flav2_, flav3_, flav4_] :> (3*(gl + g2*gM)*(g2*gM + gq)*k*(-3 + 2*L)*
    conj[ye][flav2, flav1]*conj[yu][flav4, flav3])/(32*MV^2), 

Cquqd8[flav1_, flav2_, flav3_, flav4_] :> (-3*(g2*gM + gq)^2*k*(-3 + 2*L)*yd[flav1, flav4]*yu[flav3, flav2])/
   (2*MV^2), 

conj[Cquqd8][flav1_, flav2_, flav3_, flav4_] :> (-3*(g2*gM + gq)^2*k*(-3 + 2*L)*conj[yd][flav2, flav3]*
    conj[yu][flav4, flav1])/(2*MV^2)}
