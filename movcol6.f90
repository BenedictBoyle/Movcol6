PROGRAM movcol6
  USE movmod
    IMPLICIT NONE
    INTERFACE 
       SUBROUTINE ddaspk(res,neq,t,y,yprime,tout,info,rtol,atol,&
            idid,rwork,lrw,iwork,liw,rpar,ipar,jac,psol)
         INTEGER, INTENT(IN) :: neq, lrw, liw
         INTEGER, INTENT(INOUT), DIMENSION(15) :: info
         INTEGER, INTENT(INOUT), DIMENSION(liw) :: iwork
         INTEGER, INTENT(IN), DIMENSION(7) :: ipar
         INTEGER, INTENT(OUT) :: idid
         REAL(kind=8), INTENT(IN) :: tout, rtol, atol
         REAL(kind=8), INTENT(INOUT) :: t
         REAL(kind=8), DIMENSION(lrw), INTENT(INOUT) :: rwork
         REAL(kind=8), DIMENSION(4), INTENT(INOUT) :: rpar
         REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: y, yprime
         EXTERNAL :: res, jac, psol
       END SUBROUTINE ddaspk
    END INTERFACE
    EXTERNAL res, jac, dbanja, dbanps
    CHARACTER (len=32) :: arg
    INTEGER, PARAMETER :: npts = 20, neq = npts*7 + 1, neqIC = npts*5
    INTEGER :: lrw, liw, lenwp, leniwp, lenrw, leniw
    INTEGER :: idid, ii, jj, kk, ll, mm, iii, iout, ntout, job,&
         timetrans, cons, soltype, n, m, ml, mu, method
    INTEGER, DIMENSION(:), ALLOCATABLE :: iwork
    INTEGER, DIMENSION(7) :: ipar
    INTEGER, DIMENSION(20) :: info
    REAL(kind=8) :: tau, tout, tstep, dmn, expon, dx, atol, rtol, &
         phi, s, monalpha
    REAL(kind=8), PARAMETER :: sone = 0.03376524289842475d0,&
           stwo = 0.16939530676686765d0,&
           sthree = 0.3806904069584014d0,&
           sfour = 0.6193095930415986d0,&
           sfive = 0.8306046932331324d0,&
           ssix = 0.9662347571015759d0,&
           pone = 0.0d0, ptwo =	0.0848880518607158d0,&
           pthree = 0.265575603264643d0, pfour = 0.5d0,&
           pfive = 0.7344243967353573d0,&
           psix = 0.9151119481392833d0, pseven = 1.0d0
    REAL(kind=8), DIMENSION(13), PARAMETER :: points = &
         [sone, stwo, sthree, sfour, sfive, ssix, pone, ptwo, &
         pthree, pfour, pfive, psix, pseven]
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: rwork, trumesh
    REAL(kind=8), DIMENSION(4+7*12*13+1) :: rpar
    REAL(kind=8), DIMENSION(neq) :: y, yprime
    REAL(kind=8), DIMENSION(5*npts) :: yinit
    REAL(kind=8), DIMENSION(neqIC) :: yIC, yprimeIC
    REAL(kind=8), DIMENSION(neq-6) :: mesh, soln
    CHARACTER(len=20) :: fmt, fmt2, fmtmesh

! Toggle time transformation and conservative
! collocation on and off
    timetrans = 0
    cons = 1
    soltype = 2
! method = 1 for direct (Newton), 2 for Krylov
    method = 1
    if (method == 1) then
       lenrw = 50 + 9*neq + neq**2
       leniw = 40 + 2*neq
    else if (method == 2) then
       lrw = 91+18*neq
       liw = 40+neq 
       lenwp = 43*neq+2*((neq/29)+1)
       leniwp = neq 
       lenrw = lrw +lenwp
       leniw = liw + leniwp
    end if
    
    ALLOCATE(rwork(lenrw),iwork(leniw),trumesh(npts+2))
       
    dmn = 15.0d0
    call getarg(1,arg) 
    if (arg /= '') then
      read(arg,*) expon
    else
      expon = 5.0d0
    end if
    dx = dmn/dble(npts-1)
    atol = 1.0d-3
    rtol = 1.0d-3
    monalpha = 1.0d0 ! parameter controlling mass added to monitor
    if (soltype == 1) then
       write(fmt,'("("I0,"D30.15)")') neq+1
    else if (soltype == 2) then
       write(fmt,'("("I0,"D30.15)")') neq-5
       write(fmtmesh,'("("I0,"D30.15)")') npts+2
    end if

    write(fmt2,'("("I0,"D30.15)")') neq

    do n = 0,11
       do m = 0,6
          do kk = 1,13
             s = points(kk)
             call shape_function(phi,n,m,s)
             rpar(4+91*n+13*m+kk) = phi
          end do
       end do
    end do
    
    ml = 14
    mu = 14 ! bandwidths (approximate) for preconditioner

! job = 1 for mesh initialize, job = 2 for full problem
    job = 1
    rpar(1) = expon
    rpar(2) = dx
    rpar(3) = dmn
    rpar(1097) = monalpha
    ipar(1) = ml
    ipar(2) = mu
    ipar(3) = neq
    ipar(4) = npts
    ipar(5) = job
    ipar(6) = timetrans
    ipar(7) = cons
    if (soltype == 1) then
       open(50, file = 'CH6profs1.dat')
    else if (soltype == 2) then
       open(50, file = 'CH6Sprofs2.dat')
       open(55, file = 'CH6mesh.dat')
    end if

    do ii = 1,npts
       y(7*ii) = dble(ii-1)*dx
       y(7*ii-6) = 0.0d0
       y(7*ii-5) = 0.0d0
       y(7*ii-4) = 0.0d0
       y(7*ii-3) = 0.0d0
       y(7*ii-2) = 0.0d0
       y(7*ii-1) = 0.0d0
       yprime(7*ii-6) = 0.0d0
       yprime(7*ii-5) = 0.0d0
       yprime(7*ii-4) = 0.0d0
       yprime(7*ii-3) = 0.0d0
       yprime(7*ii-2) = 0.0d0
       yprime(7*ii-1) = 0.0d0
       yprime(7*ii) = 0.0d0
    end do
    y(neq) = 0.0d0
    yprime(neq) = 0.0d0

    do jj = 1,20
       info(jj) = 0
    end do
 ! info(9) selects order of bd method, info(11) toggles ic consistency
    info(9) = 1
    info(11) = 1 ! might need to do something with iwork here
    do mm = 1,neq
       iwork(40+mm) = 1
    end do
    if (method == 2) then
       info(12) = 1 ! for krylov method
       info(15) = 1 ! dbanja exists
       iwork(27) = lenwp
       iwork(28) = leniwp ! needed for preconditioning
    end if
    iwork(3) = 1 ! order of bd algorithm

    tau = 0.0d0
    tout = 1.0d-12
    write(*,*) "Performing intial mesh calculation..."
! calculate consistent i.c. for inital mesh calc
     call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
          lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
     info(1) = 0 ! reset ddaspk
     info(11) = 0
     iwork(3) = 5
     tau = 0.0d0
     tout = 1.0d0
     y(neq) = 0.0d0
    
! perform initial mesh calc
    call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
         lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
    do while (idid == -1) 
       info(1) = 1
       call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
            lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
    end do
    ! open(55,file = 'Initprof.dat')
    ! write(55,fmt2) yIC 
    
    open(54,file = 'testinit.dat')
    
    write(54,fmt) y

    close(54)
    
    tau = 0.0d0
    y(neq) = 0.0d0
    tout = 1.0d-10
    job = 2
    ipar(5) = job
    timetrans = 1
    ipar(6) = timetrans
    rtol = 1.0d-3
    atol = 1.0d-3
    do jj = 1,20
       info(jj) = 0
    end do
 ! info(9) selects order of bd method, info(11) toggles ic consistency
    info(9) = 1
    info(11) = 1
    do mm = 1,neq
       iwork(40+mm) = 1
    end do
    if (method == 2) then
       info(12) = 1 ! for krylov method
       info(15) = 1 ! dbanja exists
       iwork(27) = lenwp
       iwork(28) = leniwp ! needed for preconditioning
    end if
    iwork(3) = 1 ! order of bd algorithm

    if (soltype == 1) then
       write(50,fmt) y, tau
    else if (soltype == 2) then
       call solution_output(neq,y,yprime,rpar,ipar,mesh,soln)
       write(50,fmt) mesh, tau
       write(50,fmt) soln, tau
    end if
! reset inital guess for consistent yprime to all zeros
    do jj = 1,npts
       yprime(7*jj-6) = 0.0d0
       yprime(7*jj-5) = 0.0d0
       yprime(7*jj-4) = 0.0d0
       yprime(7*jj-3) = 0.0d0
       yprime(7*jj-2) = 0.0d0
       yprime(7*jj-1) = 0.0d0
       yprime(7*jj) = 0.0d0
    end do
    yprime(neq) = 0.0d0
! calculate consistent ic for full problem
    write(*,*) "Solving full problem... "
    call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
         lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
    do while (idid == -1) 
       info(1) = 1
       call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
            lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
    end do

    call getarg(2,arg) 
    if (arg /= '') then
      read(arg,*) tstep
    else
      tstep = 0.1d0
    end if
    call getarg(3,arg) 
    if (arg /= '') then
      read(arg,*) ntout
    else
      ntout = 100
    end if
    info(1) = 0
    info(11) = 0
    iwork(3) = 5
    tau = 0.0d0
    tout = tstep
    y(neq) = 0.0d0
    atol = 1.0d-5
    rtol = 1.0d-5
    
    if (soltype == 1) then
       write(50,fmt) y, tau
    else if (soltype == 2) then
       do iii = 1,npts
          trumesh(iii) = y(7*iii)
       end do
       trumesh(npts+1) = y(7*npts+1)
       trumesh(npts+2) = tau
       write(55,fmtmesh) trumesh
       call solution_output(neq,y,yprime,rpar,ipar,mesh,soln)
       write(50,fmt) mesh, tau
       write(50,fmt) soln, tau
    end if
! solve full problem
    write(*,*) "Current loop: ", 1, " of ", ntout, "   Real time: ", y(neq)
    do iout = 1,ntout
       call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,&
            rwork,lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
       do while (idid == -1) 
          info(1) = 1
          call ddaspk(res,neq,tau,y,yprime,tout,info,rtol,atol,idid,rwork,&
               lenrw,iwork,leniw,rpar,ipar,dbanja,dbanps)
       end do
       if (soltype == 1) then
          write(50,fmt) y, tau
       else if (soltype == 2) then
          do iii = 1,npts
             trumesh(iii) = y(7*iii)
          end do
          trumesh(npts+1) = y(7*npts+1)
          trumesh(npts+2) = tau
          call solution_output(neq,y,yprime,rpar,ipar,mesh,soln)
          if (mod(iout,50) == 1) then
            write(50,fmt) mesh, tau
            write(50,fmt) soln, tau
            write(55,fmtmesh) trumesh
            write(*,*) "Current loop: ", iout+1, " of ", ntout, "   Real time: ", y(neq)
          end if
    end if
       tout = tout + tstep
    end do
    close(50)    
    DEALLOCATE(rwork,iwork,trumesh)
  END PROGRAM movcol6

  SUBROUTINE res(tau,y,yprime,cj,delta,ires,rpar,ipar)
    USE movmod
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: ires
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    INTEGER :: neq,  job
    REAL(kind=8), INTENT(IN) :: tau, cj
    REAL(kind=8), DIMENSION(*), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(*), INTENT(INOUT) :: delta
    neq = ipar(3)
    job = ipar(5)
    if (job == 1) then
       CALL residual_ic(neq,tau,y,yprime,delta,ires,rpar,ipar)
       CALL residual_mesheq(neq,tau,y,yprime,delta,ires,rpar,ipar)
    elseif (job == 2) then
       CALL residual_pde(neq,tau,y,yprime,delta,ires,rpar,ipar)
       CALL residual_mesheq(neq,tau,y,yprime,delta,ires,rpar,ipar)
    end if
    RETURN
  END SUBROUTINE res

  SUBROUTINE jac(t,y,yprime,pd,cj,rpar,ipar)
    IMPLICIT NONE
    INTEGER, DIMENSION(7) :: ipar
    REAL(kind=8) :: t, cj
    REAL(kind=8), DIMENSION(4+7*12*13) :: rpar
    REAL(kind=8), DIMENSION(*) :: y,yprime
    REAL(kind=8), DIMENSION(981,*) :: pd
    RETURN
  END SUBROUTINE jac
