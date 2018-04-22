# Movcol6

Movcol6 is a set of routines for simulating solutions of sixth order PDEs, specifically ones that develop singularities or other hard-to-resolve features.

This extends earlier software [Movcol](https://www.math.ku.edu/~huang/research/movcol/movcol.html) by Huang and Russell<sup>1</sup> for simulating second order PDE in divergence form, as well as Movcol4 for general fourth order PDE by Russell, Williams and Xu<sup>2</sup>. However, this is rewritten from the ground up in Fortran 90 with a number of modifications for performance and stability, the details of which will be discussed in my forthcoming PhD thesis. 

Included for convenience is the DASPK package by Petzold et. al. availble directly [here](http://www.cs.ucsb.edu/~cse/software.html).

This repository contains a working core set of modules that can be manually edited to work with different equations, boundary conditions, monitor functions etc. though it may not be obvious where these changes need to be made. Having recently resurrected the codebase, I am of course shocked at the appauling, spaghettified nature of the code though it got me the results I needed at the time. I have plans to tidy up the code and provide a nice config-file interface for passing equations, parameters and so on in order that it may be more widely useful. A plotting function will also be provided when time allows. 


[1] Huang, Weizhang(1-KS); Russell, Robert D.(3-SFR)
A moving collocation method for solving time dependent partial differential equations. (English summary)
Workshop on the method of lines for time-dependent problems (Lexington, KY, 1995).
Appl. Numer. Math. 20 (1996), no. 1-2, 101–116.
65M20 (65L06)

[2] Russell, R. D.(3-SFR); Williams, J. F.(3-SFR); Xu, X.(3-SFR)
MOVCOL4: a moving mesh code for fourth-order time-dependent partial differential equations. (English summary)
SIAM J. Sci. Comput. 29 (2007), no. 1, 197–220 (electronic).
35G25 (35B40 35K55 35Q53 65M50 65M70 76M25)
