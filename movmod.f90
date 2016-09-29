MODULE movmod
        ! Program not compiled and run since RETURN statements removed from all subroutines
CONTAINS
  SUBROUTINE shape_function(phi,j,k,s)
    ! Returns the Hermite interpolating polynomials on [0,1] and their 
    ! derivatives up to order 6
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: j, k
    REAL(kind=8), INTENT(IN) :: s
    REAL(kind=8), INTENT(OUT) :: phi

    if (j == 0) then
       if (k == 0) then
          phi = (1.0d0) - &
               (462.0d0)*(s**6.0d0) + &
               (1980.0d0)*(s**7.0d0) - &
               (3465.0d0)*(s**8.0d0) + &
               (3080.0d0)*(s**9.0d0) - &
               (1386.0d0)*(s**10.0d0) + &
               (252.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = -(2772.0d0)*(s**5.0d0) + &
               (13860.0d0)*(s**6.0d0) - &
               (27720.0d0)*(s**7.0d0) + &
               (27720.0d0)*(s**8.0d0) - &
               (13860.0d0)*(s**9.0d0) + &
               (2772.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = -(13860.0d0)*(s**4.0d0) + &
               (83160.0d0)*(s**5.0d0) - &
               (194040.0d0)*(s**6.0d0) + &
               (221760.0d0)*(s**7.0d0) - &
               (124740.0d0)*(s**8.0d0) + &
               (27720.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(55440.0d0)*(s**3.0d0) + &
               (415800.0d0)*(s**4.0d0) - &
               (1164240.0d0)*(s**5.0d0) + &
               (1552320.0d0)*(s**6.0d0) - &
               (997920.0d0)*(s**7.0d0) + &
               (249480.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(166320.0d0)*(s**2.0d0) + &
               (1663200.0d0)*(s**3.0d0) - &
               (5821200.0d0)*(s**4.0d0) + &
               (9313920.0d0)*(s**5.0d0) - &
               (6985440.0d0)*(s**6.0d0) + &
               (1995840.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(332640.0d0)*s + &
               (4989600.0d0)*(s**2.0d0) - &
               (23284800.0d0)*(s**3.0d0) + &
               (46569600.0d0)*(s**4.0d0) - &
               (41912640.0d0)*(s**5.0d0) + &
               (13970880.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(332640.0d0) + &
               (9979200.0d0)*s - &
               (69854400.0d0)*(s**2.0d0) + &
               (186278400.0d0)*(s**3.0d0) - &
               (209563200.0d0)*(s**4.0d0) + &
               (83825280.0d0)*(s**5.0d0)
       end if
    else if (j == 1) then
       if (k == 0) then
          phi = s - &
               (252.0d0)*(s**6.0d0) + &
               (1050.0d0)*(s**7.0d0) - &
               (1800.0d0)*(s**8.0d0) + &
               (1575.0d0)*(s**9.0d0) - &
               (700.0d0)*(s**10.0d0) + &
               (126.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (1.0d0) - &
               (1512.0d0)*(s**5.0d0) + &
               (7350.0d0)*(s**6.0d0) - &
               (14400.0d0)*(s**7.0d0) + &
               (14175.0d0)*(s**8.0d0) - &
               (7000.0d0)*(s**9.0d0) + &
               (1386.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = -(7560.0d0)*(s**4.0d0) + &
               (44100.0d0)*(s**5.0d0) - &
               (100800.0d0)*(s**6.0d0) + &
               (113400.0d0)*(s**7.0d0) - &
               (63000.0d0)*(s**8.0d0) + &
               (13860.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(30240.0d0)*(s**3.0d0) + &
               (220500.0d0)*(s**4.0d0) - &
               (604800.0d0)*(s**5.0d0) + &
               (793800.0d0)*(s**6.0d0) - &
               (504000.0d0)*(s**7.0d0) + &
               (124740.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(90720.0d0)*(s**2.0d0) + &
               (882000.0d0)*(s**3.0d0) - &
               (3024000.0d0)*(s**4.0d0) + &
               (4762800.0d0)*(s**5.0d0) - &
               (3528000.0d0)*(s**6.0d0) + &
               (997920.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(181440.0d0)*s + &
               (2646000.0d0)*(s**2.0d0) - &
               (12096000.0d0)*(s**3.0d0) + &
               (23814000.0d0)*(s**4.0d0) - &
               (21168000.0d0)*(s**5.0d0) + &
               (6985440.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(181440.0d0) + &
               (5292000.0d0)*s - &
               (36288000.0d0)*(s**2.0d0) + &
               (95256000.0d0)*(s**3.0d0) - &
               (105840000.0d0)*(s**4.0d0) + &
               (41912640.0d0)*(s**5.0d0)
       end if
    else if (j == 2) then
       if (k == 0) then
          phi = (1.0d0/2.0d0)*(s**2.0d0) - &
               (63.0d0)*(s**6.0d0) + &
               (252.0d0)*(s**7.0d0) - &
               (420.0d0)*(s**8.0d0) + &
               (360.0d0)*(s**9.0d0) - &
               (315.0d0/2.0d0)*(s**10.0d0) + &
               (28.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = s - &
               (378.0d0)*(s**5.0d0) + &
               (1764.0d0)*(s**6.0d0) - &
               (3360.0d0)*(s**7.0d0) + &
               (3240.0d0)*(s**8.0d0) - &
               (1575.0d0)*(s**9.0d0) + &
               (308.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (1.0d0) - &
               (1890.0d0)*(s**4.0d0) + &
               (10584.0d0)*(s**5.0d0) - &
               (23520.0d0)*(s**6.0d0) + &
               (25920.0d0)*(s**7.0d0) - &
               (14175.0d0)*(s**8.0d0) + &
               (3080.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(7560.0d0)*(s**3.0d0) + &
               (52920.0d0)*(s**4.0d0) - &
               (141120.0d0)*(s**5.0d0) + &
               (181440.0d0)*(s**6.0d0) - &
               (113400.0d0)*(s**7.0d0) + &
               (27720.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(22680.0d0)*(s**2.0d0) + &
               (211680.0d0)*(s**3.0d0) - &
               (705600.0d0)*(s**4.0d0) + &
               (1088640.0d0)*(s**5.0d0) - &
               (793800.0d0)*(s**6.0d0) + &
               (221760.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(45360.0d0)*s + &
               (635040.0d0)*(s**2.0d0) - &
               (2822400.0d0)*(s**3.0d0) + &
               (5443200.0d0)*(s**4.0d0) - &
               (4762800.0d0)*(s**5.0d0) + &
               (1552320.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(45360.0d0) + &
               (1270080.0d0)*s - &
               (8467200.0d0)*(s**2.0d0) + &
               (21772800.0d0)*(s**3.0d0) - &
               (23814000.0d0)*(s**4.0d0) + &
               (9313920.0d0)*(s**5.0d0)
       end if
    else if (j == 3) then
       if (k == 0) then
          phi = (1.0d0/6.0d0)*(s**3.0d0) - &
               (28.0d0/3.0d0)*(s**6.0d0) + &
               (35.0d0)*(s**7.0d0) - &
               (56.0d0)*(s**8.0d0) + &
               (140.0d0/3.0d0)*(s**9.0d0) - &
               (20.0d0)*(s**10.0d0) + &
               (7.0d0/2.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (1.0d0/2.0d0)*(s**2.0d0) - &
               (56.0d0)*(s**5.0d0) + &
               (245.0d0)*(s**6.0d0) - &
               (448.0d0)*(s**7.0d0) + &
               (420.0d0)*(s**8.0d0) - &
               (200.0d0)*(s**9.0d0) + &
               (77.0d0/2.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = s - &
               (280.0d0)*(s**4.0d0) + &
               (1470.0d0)*(s**5.0d0) - &
               (3136.0d0)*(s**6.0d0) + &
               (3360.0d0)*(s**7.0d0) - &
               (1800.0d0)*(s**8.0d0) + &
               (385.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = (1.0d0) - &
               (1120.0d0)*(s**3.0d0) + &
               (7350.0d0)*(s**4.0d0) - &
               (18816.0d0)*(s**5.0d0) + &
               (23520.0d0)*(s**6.0d0) - &
               (14400.0d0)*(s**7.0d0) + &
               (3465.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(3360.0d0)*(s**2.0d0) + &
               (29400.0d0)*(s**3.0d0) - &
               (94080.0d0)*(s**4.0d0) + &
               (141120.0d0)*(s**5.0d0) - &
               (100800.0d0)*(s**6.0d0) + &
               (27720.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(6720.0d0)*s + &
               (88200.0d0)*(s**2.0d0) - &
               (376320.0d0)*(s**3.0d0) + &
               (705600.0d0)*(s**4.0d0) - &
               (604800.0d0)*(s**5.0d0) + &
               (194040.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(6720.0d0) + &
               (176400.0d0)*s - &
               (1128960.0d0)*(s**2.0d0) + &
               (2822400.0d0)*(s**3.0d0) - &
               (3024000.0d0)*(s**4.0d0) + &
               (1164240.0d0)*(s**5.0d0)
       end if
    else if (j == 4) then
       if (k == 0) then
          phi = (1.0d0/24.0d0)*(s**4.0d0) - &
               (7.0d0/8.0d0)*(s**6.0d0) + &
               (35.0d0/12.0d0)*(s**7.0d0) - &
               (35.0d0/8.0d0)*(s**8.0d0) + &
               (7.0d0/2.0d0)*(s**9.0d0) - &
               (35.0d0/24.0d0)*(s**10.0d0) + &
               (1.0d0/4.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (1.0d0/6.0d0)*(s**3.0d0) - &
               (21.0d0/4.0d0)*(s**5.0d0) + &
               (245.0d0/12.0d0)*(s**6.0d0) - &
               (35.0d0)*(s**7.0d0) + &
               (63.0d0/2.0d0)*(s**8.0d0) - &
               (175.0d0/12.0d0)*(s**9.0d0) + &
               (11.0d0/4.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (1.0d0/2.0d0)*(s**2.0d0) - &
               (105.0d0/4.0d0)*(s**4.0d0) + &
               (245.0d0/2.0d0)*(s**5.0d0) - &
               (245.0d0)*(s**6.0d0) + &
               (252.0d0)*(s**7.0d0) - &
               (525.0d0/4.0d0)*(s**8.0d0) + &
               (55.0d0/2.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = s - &
               (105.0d0)*(s**3.0d0) + &
               (1225.0d0/2.0d0)*(s**4.0d0) - &
               (1470.0d0)*(s**5.0d0) + &
               (1764.0d0)*(s**6.0d0) - &
               (1050.0d0)*(s**7.0d0) + &
               (495.0d0/2.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = (1.0d0) - &
               (315.0d0)*(s**2.0d0) + &
               (2450.0d0)*(s**3.0d0) - &
               (7350.0d0)*(s**4.0d0) + &
               (10584.0d0)*(s**5.0d0) - &
               (7350.0d0)*(s**6.0d0) + &
               (1980.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(630.0d0)*s + &
               (7350.0d0)*(s**2.0d0) - &
               (29400.0d0)*(s**3.0d0) + &
               (52920.0d0)*(s**4.0d0) - &
               (44100.0d0)*(s**5.0d0) + &
               (13860.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(630.0d0) + &
               (14700.0d0)*s - &
               (88200.0d0)*(s**2.0d0) + &
               (211680.0d0)*(s**3.0d0) - &
               (220500.0d0)*(s**4.0d0) + &
               (83160.0d0)*(s**5.0d0)
       end if
    else if (j == 5) then
       if (k == 0) then
          phi = (1.0d0/120.0d0)*(s**5.0d0) - &
               (1.0d0/20.0d0)*(s**6.0d0) + &
               (1.0d0/8.0d0)*(s**7.0d0) - &
               (1.0d0/6.0d0)*(s**8.0d0) + &
               (1.0d0/8.0d0)*(s**9.0d0) - &
               (1.0d0/20.0d0)*(s**10.0d0) + &
               (1.0d0/120.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (1.0d0/24.0d0)*(s**4.0d0) - &
               (3.0d0/10.0d0)*(s**5.0d0) + &
               (7.0d0/8.0d0)*(s**6.0d0) - &
               (4.0d0/3.0d0)*(s**7.0d0) + &
               (9.0d0/8.0d0)*(s**8.0d0) - &
               (1.0d0/2.0d0)*(s**9.0d0) + &
               (11.0d0/120.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (1.0d0/6.0d0)*(s**3.0d0) - &
               (3.0d0/2.0d0)*(s**4.0d0) + &
               (21.0d0/4.0d0)*(s**5.0d0) - &
               (28.0d0/3.0d0)*(s**6.0d0) + &
               (9.0d0)*(s**7.0d0) - &
               (9.0d0/2.0d0)*(s**8.0d0) + &
               (11.0d0/12.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = (1.0d0/2.0d0)*(s**2.0d0) - &
               (6.0d0)*(s**3.0d0) + &
               (105.0d0/4.0d0)*(s**4.0d0) - &
               (56.0d0)*(s**5.0d0) + &
               (63.0d0)*(s**6.0d0) - &
               (36.0d0)*(s**7.0d0) + &
               (33.0d0/4.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = s - &
               (18.0d0)*(s**2.0d0) + &
               (105.0d0)*(s**3.0d0) - &
               (280.0d0)*(s**4.0d0) + &
               (378.0d0)*(s**5.0d0) - &
               (252.0d0)*(s**6.0d0) + &
               (66.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = (1.0d0) - &
               (36.0d0)*s + &
               (315.0d0)*(s**2.0d0) - &
               (1120.0d0)*(s**3.0d0) + &
               (1890.0d0)*(s**4.0d0) - &
               (1512.0d0)*(s**5.0d0) + &
               (462.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(36.0d0) + &
               (630.0d0)*s - &
               (3360.0d0)*(s**2.0d0) + &
               (7560.0d0)*(s**3.0d0) - &
               (7560.0d0)*(s**4.0d0) + &
               (2772.0d0)*(s**5.0d0)
       end if
    else if (j == 6) then
       if (k == 0) then
          phi = (462.0d0)*(s**6.0d0) - &
               (1980.0d0)*(s**7.0d0) + &
               (3465.0d0)*(s**8.0d0) - &
               (3080.0d0)*(s**9.0d0) + &
               (1386.0d0)*(s**10.0d0) - &
               (252.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (2772.0d0)*(s**5.0d0) - &
               (13860.0d0)*(s**6.0d0) + &
               (27720.0d0)*(s**7.0d0) - &
               (27720.0d0)*(s**8.0d0) + &
               (13860.0d0)*(s**9.0d0) - &
               (2772.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (13860.0d0)*(s**4.0d0) - &
               (83160.0d0)*(s**5.0d0) + &
               (194040.0d0)*(s**6.0d0) - &
               (221760.0d0)*(s**7.0d0) + &
               (124740.0d0)*(s**8.0d0) - &
               (27720.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = (55440.0d0)*(s**3.0d0) - &
               (415800.0d0)*(s**4.0d0) + &
               (1164240.0d0)*(s**5.0d0) - &
               (1552320.0d0)*(s**6.0d0) + &
               (997920.0d0)*(s**7.0d0) - &
               (249480.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = (166320.0d0)*(s**2.0d0) - &
               (1663200.0d0)*(s**3.0d0) + &
               (5821200.0d0)*(s**4.0d0) - &
               (9313920.0d0)*(s**5.0d0) + &
               (6985440.0d0)*(s**6.0d0) - &
               (1995840.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = (332640.0d0)*s - &
               (4989600.0d0)*(s**2.0d0) + &
               (23284800.0d0)*(s**3.0d0) - &
               (46569600.0d0)*(s**4.0d0) + &
               (41912640.0d0)*(s**5.0d0) - &
               (13970880.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = (332640.0d0) - &
               (9979200.0d0)*s + &
               (69854400.0d0)*(s**2.0d0) - &
               (186278400.0d0)*(s**3.0d0) + &
               (209563200.0d0)*(s**4.0d0) - &
               (83825280.0d0)*(s**5.0d0)
       end if
    else if (j == 7) then
       if (k == 0) then
          phi = -(210.0d0)*(s**6.0d0) + &
               (930.0d0)*(s**7.0d0) - &
               (1665.0d0)*(s**8.0d0) + &
               (1505.0d0)*(s**9.0d0) - &
               (686.0d0)*(s**10.0d0) + &
               (126.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = -(1260.0d0)*(s**5.0d0) + &
               (6510.0d0)*(s**6.0d0) - &
               (13320.0d0)*(s**7.0d0) + &
               (13545.0d0)*(s**8.0d0) - &
               (6860.0d0)*(s**9.0d0) + &
               (1386.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = -(6300.0d0)*(s**4.0d0) + &
               (39060.0d0)*(s**5.0d0) - &
               (93240.0d0)*(s**6.0d0) + &
               (108360.0d0)*(s**7.0d0) - &
               (61740.0d0)*(s**8.0d0) + &
               (13860.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(25200.0d0)*(s**3.0d0) + &
               (195300.0d0)*(s**4.0d0) - &
               (559440.0d0)*(s**5.0d0) + &
               (758520.0d0)*(s**6.0d0) - &
               (493920.0d0)*(s**7.0d0) + &
               (124740.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(75600.0d0)*(s**2.0d0) + &
               (781200.0d0)*(s**3.0d0) - &
               (2797200.0d0)*(s**4.0d0) + &
               (4551120.0d0)*(s**5.0d0) - &
               (3457440.0d0)*(s**6.0d0) + &
               (997920.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(151200.0d0)*s + &
               (2343600.0d0)*(s**2.0d0) - &
               (11188800.0d0)*(s**3.0d0) + &
               (22755600.0d0)*(s**4.0d0) - &
               (20744640.0d0)*(s**5.0d0) + &
               (6985440.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(151200.0d0) + &
               (4687200.0d0)*s - &
               (33566400.0d0)*(s**2.0d0) + &
               (91022400.0d0)*(s**3.0d0) - &
               (103723200.0d0)*(s**4.0d0) + &
               (41912640.0d0)*(s**5.0d0)
       end if
    else if (j == 8) then
       if (k == 0) then
          phi = (42.0d0)*(s**6.0d0) - &
               (192.0d0)*(s**7.0d0) + &
               (705.0d0/2.0d0)*(s**8.0d0) - &
               (325.0d0)*(s**9.0d0) + &
               (301.0d0/2.0d0)*(s**10.0d0) - &
               (28.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (252.0d0)*(s**5.0d0) - &
               (1344.0d0)*(s**6.0d0) + &
               (2820.0d0)*(s**7.0d0) - &
               (2925.0d0)*(s**8.0d0) + &
               (1505.0d0)*(s**9.0d0) - &
               (308.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (1260.0d0)*(s**4.0d0) - &
               (8064.0d0)*(s**5.0d0) + &
               (19740.0d0)*(s**6.0d0) - &
               (23400.0d0)*(s**7.0d0) + &
               (13545.0d0)*(s**8.0d0) - &
               (3080.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = (5040.0d0)*(s**3.0d0) - &
               (40320.0d0)*(s**4.0d0) + &
               (118440.0d0)*(s**5.0d0) - &
               (163800.0d0)*(s**6.0d0) + &
               (108360.0d0)*(s**7.0d0) - &
               (27720.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = (15120.0d0)*(s**2.0d0) - &
               (161280.0d0)*(s**3.0d0) + &
               (592200.0d0)*(s**4.0d0) - &
               (982800.0d0)*(s**5.0d0) + &
               (758520.0d0)*(s**6.0d0) - &
               (221760.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = (30240.0d0)*s - &
               (483840.0d0)*(s**2.0d0) + &
               (2368800.0d0)*(s**3.0d0) - &
               (4914000.0d0)*(s**4.0d0) + &
               (4551120.0d0)*(s**5.0d0) - &
               (1552320.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = (30240.0d0) - &
               (967680.0d0)*s + &
               (7106400.0d0)*(s**2.0d0) - &
               (19656000.0d0)*(s**3.0d0) + &
               (22755600.0d0)*(s**4.0d0) - &
               (9313920.0d0)*(s**5.0d0)
       end if
    else if (j == 9) then
       if (k == 0) then
          phi = -(14.0d0/3.0d0)*(s**6.0d0) + &
               (22.0d0)*(s**7.0d0) - &
               (83.0d0/2.0d0)*(s**8.0d0) + &
               (235.0d0/6.0d0)*(s**9.0d0) - &
               (37.0d0/2.0d0)*(s**10.0d0) + &
               (7.0d0/2.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = -(28.0d0)*(s**5.0d0) + &
               (154.0d0)*(s**6.0d0) - &
               (332.0d0)*(s**7.0d0) + &
               (705.0d0/2.0d0)*(s**8.0d0) - &
               (185.0d0)*(s**9.0d0) + &
               (77.0d0/2.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = -(140.0d0)*(s**4.0d0) + &
               (924.0d0)*(s**5.0d0) - &
               (2324.0d0)*(s**6.0d0) + &
               (2820.0d0)*(s**7.0d0) - &
               (1665.0d0)*(s**8.0d0) + &
               (385.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(560.0d0)*(s**3.0d0) + &
               (4620.0d0)*(s**4.0d0) - &
               (13944.0d0)*(s**5.0d0) + &
               (19740.0d0)*(s**6.0d0) - &
               (13320.0d0)*(s**7.0d0) + &
               (3465.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(1680.0d0)*(s**2.0d0) + &
               (18480.0d0)*(s**3.0d0) - &
               (69720.0d0)*(s**4.0d0) + &
               (118440.0d0)*(s**5.0d0) - &
               (93240.0d0)*(s**6.0d0) + &
               (27720.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(3360.0d0)*s + &
               (55440.0d0)*(s**2.0d0) - &
               (278880.0d0)*(s**3.0d0) + &
               (592200.0d0)*(s**4.0d0) - &
               (559440.0d0)*(s**5.0d0) + &
               (194040.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(3360.0d0) + &
               (110880.0d0)*s - &
               (836640.0d0)*(s**2.0d0) + &
               (2368800.0d0)*(s**3.0d0) - &
               (2797200.0d0)*(s**4.0d0) + &
               (1164240.0d0)*(s**5.0d0)
       end if
    else if (j == 10) then
       if (k == 0) then
          phi = (7.0d0/24.0d0)*(s**6.0d0) - &
               (17.0d0/12.0d0)*(s**7.0d0) + &
               (11.0d0/4.0d0)*(s**8.0d0) - &
               (8.0d0/3.0d0)*(s**9.0d0) + &
               (31.0d0/24.0d0)*(s**10.0d0) - &
               (1.0d0/4.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = (7.0d0/4.0d0)*(s**5.0d0) - &
               (119.0d0/12.0d0)*(s**6.0d0) + &
               (22.0d0)*(s**7.0d0) - &
               (24.0d0)*(s**8.0d0) + &
               (155.0d0/12.0d0)*(s**9.0d0) - &
               (11.0d0/4.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = (35.0d0/4.0d0)*(s**4.0d0) - &
               (119.0d0/2.0d0)*(s**5.0d0) + &
               (154.0d0)*(s**6.0d0) - &
               (192.0d0)*(s**7.0d0) + &
               (465.0d0/4.0d0)*(s**8.0d0) - &
               (55.0d0/2.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = (35.0d0)*(s**3.0d0) - &
               (595.0d0/2.0d0)*(s**4.0d0) + &
               (924.0d0)*(s**5.0d0) - &
               (1344.0d0)*(s**6.0d0) + &
               (930.0d0)*(s**7.0d0) - &
               (495.0d0/2.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = (105.0d0)*(s**2.0d0) - &
               (1190.0d0)*(s**3.0d0) + &
               (4620.0d0)*(s**4.0d0) - &
               (8064.0d0)*(s**5.0d0) + &
               (6510.0d0)*(s**6.0d0) - &
               (1980.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = (210.0d0)*s - &
               (3570.0d0)*(s**2.0d0) + &
               (18480.0d0)*(s**3.0d0) - &
               (40320.0d0)*(s**4.0d0) + &
               (39060.0d0)*(s**5.0d0) - &
               (13860.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = (210.0d0) - &
               (7140.0d0)*s + &
               (55440.0d0)*(s**2.0d0) - &
               (161280.0d0)*(s**3.0d0) + &
               (195300.0d0)*(s**4.0d0) - &
               (83160.0d0)*(s**5.0d0)
       end if
    else if (j == 11) then
       if (k == 0) then
          phi = -(1.0d0/120.0d0)*(s**6.0d0) + &
               (1.0d0/24.0d0)*(s**7.0d0) - &
               (1.0d0/12.0d0)*(s**8.0d0) + &
               (1.0d0/12.0d0)*(s**9.0d0) - &
               (1.0d0/24.0d0)*(s**10.0d0) + &
               (1.0d0/120.0d0)*(s**11.0d0)
       else if (k == 1) then
          phi = -(1.0d0/20.0d0)*(s**5.0d0) + &
               (7.0d0/24.0d0)*(s**6.0d0) - &
               (2.0d0/3.0d0)*(s**7.0d0) + &
               (3.0d0/4.0d0)*(s**8.0d0) - &
               (5.0d0/12.0d0)*(s**9.0d0) + &
               (11.0d0/120.0d0)*(s**10.0d0)
       else if (k == 2) then
          phi = -(1.0d0/4.0d0)*(s**4.0d0) + &
               (7.0d0/4.0d0)*(s**5.0d0) - &
               (14.0d0/3.0d0)*(s**6.0d0) + &
               (6.0d0)*(s**7.0d0) - &
               (15.0d0/4.0d0)*(s**8.0d0) + &
               (11.0d0/12.0d0)*(s**9.0d0)
       else if (k == 3) then
          phi = -(s**3.0d0) + &
               (35.0d0/4.0d0)*(s**4.0d0) - &
               (28.0d0)*(s**5.0d0) + &
               (42.0d0)*(s**6.0d0) - &
               (30.0d0)*(s**7.0d0) + &
               (33.0d0/4.0d0)*(s**8.0d0)
       else if (k == 4) then
          phi = -(3.0d0)*(s**2.0d0) + &
               (35.0d0)*(s**3.0d0) - &
               (140.0d0)*(s**4.0d0) + &
               (252.0d0)*(s**5.0d0) - &
               (210.0d0)*(s**6.0d0) + &
               (66.0d0)*(s**7.0d0)
       else if (k == 5) then
          phi = -(6.0d0)*s + &
               (105.0d0)*(s**2.0d0) - &
               (560.0d0)*(s**3.0d0) + &
               (1260.0d0)*(s**4.0d0) - &
               (1260.0d0)*(s**5.0d0) + &
               (462.0d0)*(s**6.0d0)
       else if (k == 6) then
          phi = -(6.0d0) + &
               (210.0d0)*s - &
               (1680.0d0)*(s**2.0d0) + &
               (5040.0d0)*(s**3.0d0) - &
               (6300.0d0)*(s**4.0d0) + &
               (2772.0d0)*(s**5.0d0)
       end if
    end if


  END SUBROUTINE shape_function

  SUBROUTINE derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
       uxxxx,uxxxxx,uxxxxxx,utau,rpar)
    ! return value of u and its spatio-termporal derivatives
    ! at a point x_i < x < x_i+1
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i, neq, x
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), INTENT(OUT) :: u, ux, uxx, uxxx, uxxxx,&
         uxxxxx, uxxxxxx, utau
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), PARAMETER :: sone = 0.03376524289842475d0,&
         stwo = 0.16939530676686765d0,&
         sthree = 0.3806904069584014d0,&
         sfour = 0.6193095930415986d0,&
         sfive = 0.8306046932331324d0,&
         ssix = 0.9662347571015759d0,&
         pone = 0.0d0, ptwo = 0.0848880518607158d0,&
         pthree = 0.265575603264643d0, pfour = 0.5d0,&
         pfive = 0.7344243967353573d0,&
         psix = 0.9151119481392833d0, pseven = 1.0d0
    REAL(kind=8), DIMENSION(13), PARAMETER :: points = &
         [sone, stwo, sthree, sfour, sfive, ssix, pone, ptwo, &
         pthree, pfour, pfive, psix, pseven]
    REAL(kind=8) :: h, htau

    h = y(7*(i+1)) - y(7*i)
    htau = yprime(7*(i+1)) - yprime(7*i)

    ! rpar set up so that rpar(4+1) = phi(0,0,sone), rpar(4+14) = 
    ! phi(0,1,sone), rpar(4+92) = phi(1,0,sone). Use x now to contain
    ! an integer where 1 to 6 correspond to s's and 7 to 13 
    ! correspond to p's

    u = y(7*(i-1)+1)*rpar(4+x) + &
         y(7*(i-1)+2)*rpar(4+91+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+x) + &
         y(7*i+2)*rpar(4+7*91+x)*h +&
         y(7*i+3)*rpar(4+8*91+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+x)*(h**5.0d0)

    ux = (y(7*(i-1)+1)*rpar(4+13+x) + &
         y(7*(i-1)+2)*rpar(4+91+13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+13+x) + &
         y(7*i+2)*rpar(4+7*91+13+x)*h +&
         y(7*i+3)*rpar(4+8*91+13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+13+x)*(h**5.0d0))/h

    uxx = (y(7*(i-1)+1)*rpar(4+2*13+x) + &
         y(7*(i-1)+2)*rpar(4+91+2*13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+2*13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+2*13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+2*13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+2*13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+2*13+x) + &
         y(7*i+2)*rpar(4+7*91+2*13+x)*h +&
         y(7*i+3)*rpar(4+8*91+2*13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+2*13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+2*13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+2*13+x)*(h**5.0d0))/(h**2.0d0)

    uxxx = (y(7*(i-1)+1)*rpar(4+3*13+x) + &
         y(7*(i-1)+2)*rpar(4+91+3*13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+3*13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+3*13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+3*13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+3*13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+3*13+x) + &
         y(7*i+2)*rpar(4+7*91+3*13+x)*h +&
         y(7*i+3)*rpar(4+8*91+3*13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+3*13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+3*13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+3*13+x)*(h**5.0d0))/(h**3.0d0) 

    uxxxx = (y(7*(i-1)+1)*rpar(4+4*13+x) + &
         y(7*(i-1)+2)*rpar(4+91+4*13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+4*13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+4*13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+4*13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+4*13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+4*13+x) + &
         y(7*i+2)*rpar(4+7*91+4*13+x)*h +&
         y(7*i+3)*rpar(4+8*91+4*13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+4*13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+4*13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+4*13+x)*(h**5.0d0))/(h**4.0d0)

    uxxxxx = (y(7*(i-1)+1)*rpar(4+5*13+x) + &
         y(7*(i-1)+2)*rpar(4+91+5*13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+5*13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+5*13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+5*13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+5*13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+5*13+x) + &
         y(7*i+2)*rpar(4+7*91+5*13+x)*h +&
         y(7*i+3)*rpar(4+8*91+5*13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+5*13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+5*13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+5*13+x)*(h**5.0d0))/(h**5.0d0)

    uxxxxxx = (y(7*(i-1)+1)*rpar(4+6*13+x) + &
         y(7*(i-1)+2)*rpar(4+91+6*13+x)*h +&
         y(7*(i-1)+3)*rpar(4+2*91+6*13+x)*(h**2.0d0) +&
         y(7*(i-1)+4)*rpar(4+3*91+6*13+x)*(h**3.0d0) +&
         y(7*(i-1)+5)*rpar(4+4*91+6*13+x)*(h**4.0d0) +&
         y(7*(i-1)+6)*rpar(4+5*91+6*13+x)*(h**5.0d0) +&
         y(7*i+1)*rpar(4+6*91+6*13+x) + &
         y(7*i+2)*rpar(4+7*91+6*13+x)*h +&
         y(7*i+3)*rpar(4+8*91+6*13+x)*(h**2.0d0) +&
         y(7*i+4)*rpar(4+9*91+6*13+x)*(h**3.0d0) +&
         y(7*i+5)*rpar(4+10*91+6*13+x)*(h**4.0d0) +&
         y(7*i+6)*rpar(4+11*91+6*13+x)*(h**5.0d0))/(h**6.0d0)

    utau = yprime(7*(i-1)+1)*rpar(4+x) + &
         (yprime(7*(i-1)+2)*h)*rpar(4+91+x) + &
         (yprime(7*(i-1)+3)*(h**2.0d0))*rpar(4+2*91+x) + &
         (yprime(7*(i-1)+4)*(h**3.0d0))*rpar(4+3*91+x) +&
         (yprime(7*(i-1)+5)*(h**4.0d0))*rpar(4+4*91+x) +&
         (yprime(7*(i-1)+6)*(h**5.0d0))*rpar(4+5*91+x) +&
         yprime(7*i+1)*rpar(4+6*91+x) + &
         (yprime(7*i+2)*h)*rpar(4+7*91+x) +&
         (yprime(7*i+3)*(h**2.0d0))*rpar(4+8*91+x) +&
         (yprime(7*i+4)*(h**3.0d0))*rpar(4+9*91+x) +&
         (yprime(7*i+5)*(h**4.0d0))*rpar(4+10*91+x) +&
         (yprime(7*i+6)*(h**5.0d0))*rpar(4+11*91+x) -&
         ux*(yprime(7*i)+points(x)*htau)

  END SUBROUTINE derivatives

  SUBROUTINE residual_pde(neq,tau,y,yprime,delta,ires,rpar,ipar)
    ! Fill the entries of the residual corresponding to the solution and
    ! its derivatives with the collocation equation (non conservative)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    INTEGER :: npts, ii, jj, kk, ll, timetrans, cons, x
    REAL(kind=8) :: u, ux, uxx, uxxx, uxxxx, uxxxxx, uxxxxxx, &
         utau, vx, xtau, p, g
    INTEGER, INTENT(INOUT) :: ires
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    REAL(kind=8) :: h, htau, sund, big
    REAL(kind=8), INTENT(IN) :: tau
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: delta
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: fmntr, fsmooth, prmtr
    REAL(kind=8), PARAMETER :: sone = 0.03376524289842475d0,&
         stwo = 0.16939530676686765d0,&
         sthree = 0.3806904069584014d0,&
         sfour = 0.6193095930415986d0,&
         sfive = 0.8306046932331324d0,&
         ssix = 0.9662347571015759d0
     REAL(kind=8), DIMENSION(6), PARAMETER :: points = &
         [sone, stwo, sthree, sfour, sfive, ssix]

    timetrans = ipar(6)
    cons = ipar(7)
    npts = ipar(4)
    p = rpar(1)

    ALLOCATE (fmntr(neq),fsmooth(neq),prmtr(neq))
    call monitor(neq,y,yprime,fmntr,prmtr,rpar,ipar)
    big = 0.0d0
    do kk = 1,npts
       if (fmntr(7*kk) > big) then
          big = fmntr(7*kk)
       end if
    end do
    if (timetrans==1) then
       sund = 1.0d0/big
    elseif (timetrans==0) then
       sund = 1.0d0
    end if
    if (cons == 0) then
       do ii = 1,npts-1
          h = y(7*(ii+1))-y(7*ii)
          htau = yprime(7*(ii+1)) - yprime(7*ii)
          do jj = 1,6
             call derivatives(neq,ii,jj,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                  uxxxxxx,rpar,ipar)
! one of these plus the two following lines should be uncommented 
! to attempt to stabilize the problem
             call upwinder(neq,ii,jj,y,yprime,u,vx,rpar)
             ! call lower_order(neq,ii,jj,y,yprime,vx)
             xtau = yprime(7*ii) + points(jj)*htau
             utau = utau + (ux - vx)*xtau
             if (jj < 4) then
                delta(7*(ii-1)+3+jj) = utau - sund*g
             else
                delta(7*ii+jj-3) = utau - sund*g
             end if
          end do
       end do
    else if (cons == 1) then
       do ii = 1,npts-1
          h = y(7*(ii+1))-y(7*ii)
          do jj = 1,6
             call derivatives(neq,ii,jj,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                  uxxxxxx,rpar,ipar)
             ! call upwinder(neq,ii,jj,y,yprime,u,vx,rpar)
             ! call lower_order(neq,ii,jj,y,yprime,vx)
             ! xtau = yprime(7*ii) + points(jj)*htau
             ! utau = utau + (ux - vx)*xtau
             if (jj < 4) then
                delta(7*(ii-1)+3+jj) = h*utau/sund
             else
                delta(7*ii+jj-3) = h*utau/sund
             end if
          end do
          do ll = 1,7
             if (ll == 1) then
                x = 7
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) +&
                     0.128882066524507657D2*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) -&
                     0.732825716500196678D0*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) + &
                     0.164922049039696650D0*g
                delta(7*ii+1) = delta(7*ii+1) - &
                     0.623170625543232604D-1*g
                delta(7*ii+2) = delta(7*ii+2) + &
                     0.304799806699240471D-1*g
                delta(7*ii+3) = delta(7*ii+3) - &
                     0.157386303787899057D-1*g
             else if (ll == 2) then
                x = 8
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) -&
                     0.135555128881389990D2*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) +&
                     0.709951339386726676D1*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) - &
                     0.658616272480538112D0*g
                delta(7*ii+1) = delta(7*ii+1) + &
                     0.201775206853726796D0*g
                delta(7*ii+2) = delta(7*ii+2) - &
                     0.911735577624964294D-1*g
                delta(7*ii+3) = delta(7*ii+3) + &
                     0.456091312690583006D-1*g
             else if (ll == 3) then
                x = 9
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) +&
                     0.823360561753947895D0*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) -&
                     0.684470480798240999D1*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) + &
                     0.543104912697367581D1*g
                delta(7*ii+1) = delta(7*ii+1) - &
                     0.575165066473151465D0*g
                delta(7*ii+2) = delta(7*ii+2) + &
                     0.198328593004358011D0*g
                delta(7*ii+3) = delta(7*ii+3) - &
                     0.901243497969793084D-1*g
             else if (ll == 4) then
                x = 10
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) - &
                     0.216308174972419803D0*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) +  &
                     0.615652146527092881D0*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) - &
                     0.537306182570658031D1*g
                delta(7*ii+1) = delta(7*ii+1) + &
                     0.537306182570662916D1*g
                delta(7*ii+2) = delta(7*ii+2) - &
                     0.615652146527116750D0*g
                delta(7*ii+3) = delta(7*ii+3) + &
                     0.216308174972481171D0*g
             else if (ll == 5) then
                x = 11
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) + &
                     0.901243497969332896D-1*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) -  &
                     0.198328593004349574D0*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) + &
                     0.575165066473167785D0*g
                delta(7*ii+1) = delta(7*ii+1) - &
                     0.543104912697373088D1*g
                delta(7*ii+2) = delta(7*ii+2) + &
                     0.684470480798244640D1*g
                delta(7*ii+3) = delta(7*ii+3) - &
                     0.823360561754134634D0*g
             else if (ll == 6) then
                x = 12
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) - &
                     0.456091312689924505D-1*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) +  &
                     0.911735577625287091D-1*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) - &
                     0.201775206853745059D0*g
                delta(7*ii+1) = delta(7*ii+1) + &
                     0.658616272480554432D0*g
                delta(7*ii+2) = delta(7*ii+2) - &
                     0.709951339386726943D1*g
                delta(7*ii+3) = delta(7*ii+3) + &
                     0.135555128881391269D2*g
             else if (ll == 7) then
                x = 13
                call derivatives(neq,ii,x,y,yprime,u,ux,&
                     uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
                call def_pde(g,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
                     uxxxxxx,rpar,ipar)
                delta(7*(ii-1)+4) = delta(7*(ii-1)+4) + &
                     0.157386303787635622D-1*g
                delta(7*(ii-1)+5) = delta(7*(ii-1)+5) -  &
                     0.304799806699315723D-1*g
                delta(7*(ii-1)+6) = delta(7*(ii-1)+6) + &
                     0.623170625543236212D-1*g
                delta(7*ii+1) = delta(7*ii+1) - &
                     0.164922049039704727D0*g
                delta(7*ii+2) = delta(7*ii+2) + &
                     0.732825716500154489D0*g
                delta(7*ii+3) = delta(7*ii+3) - &
                     0.128882066524507621D2*g
             end if
          end do
       end do
    end if
    ! Left Boundary Conditions
    x = 7
    call derivatives(neq,1,x,y,yprime,u,ux,uxx,uxxx,uxxxx,uxxxxx,&
         uxxxxxx,utau,rpar)
    delta(1) = ux
    delta(2) = uxxx
    delta(3) = uxxxxx

    ! Right Boundary Conditions
    x = 13 
    call derivatives(neq,npts-1,x,y,yprime,u,ux,uxx,uxxx,uxxxx,&
         uxxxxx,uxxxxxx,utau,rpar)
    delta(7*npts-3) = ux
    delta(7*npts-2) = uxxx
    delta(7*npts-1) = uxxxxx
    ! last entry contains ode for time transformation dt/dtau = g(u)
    delta(neq) = yprime(neq) - sund
    DEALLOCATE(fmntr)
  END SUBROUTINE residual_pde

  SUBROUTINE residual_mesheq(neq,tau,y,yprime,delta,ires,rpar,ipar)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    INTEGER :: npts, j, kk,  timetrans
    REAL(kind=8) ::  epsilon, h, sund, big, p
    INTEGER, INTENT(INOUT) :: ires
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    REAL(kind=8), INTENT(IN) :: tau
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: delta
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: fmntr, fsmooth, prmtr
    npts = ipar(4)
    h = rpar(2)
    p = rpar(1)
    epsilon = 1.0d-3
    ALLOCATE (fmntr(neq),fsmooth(neq),prmtr(neq))
    call monitor(neq,y,yprime,fmntr,prmtr,rpar,ipar)
    call smooth_monitor(neq,fmntr,fsmooth,rpar,ipar)
    big = 0.0d0
    do kk = 1,npts
       if (fmntr(7*kk) > big) then
          big = fmntr(7*kk)
       end if
    end do
    if (timetrans==1) then
       sund = 1.0d0/big
    elseif (timetrans==0) then
       sund = 1.0d0
    end if
    ! Fix left mesh point location by setting RHS of MEQ to zero
    delta(7) = yprime(7)/(h**2.0d0)
    do j = 2,npts-1
       delta(7*j) = (yprime(7*(j-1)) - 2.0d0*yprime(7*j) + &
            yprime(7*(j+1)))/(h**2.0d0) + &
            sund*((((fsmooth(7*(j+1)) + &
            fsmooth(7*j))*(y(7*(j+1)) - y(7*j))) - &
            (((fsmooth(7*j) + &
            fsmooth(7*(j-1)))*(y(7*j) - &
            y(7*(j-1))))))/(epsilon*(h**2.0d0)))
    end do
    ! Likewise right mesh point
    delta(7*npts) = yprime(7*npts)/(h**2.0d0)
    DEALLOCATE(fmntr,fsmooth,prmtr)
  END SUBROUTINE residual_mesheq

  SUBROUTINE monitor(neq,y,yprime,fmntr,prmtr,rpar,ipar)
    ! Calculates Monitor Function evaluated at mesh points (might be
    ! advantageous in future to figure out how to take weighted average
    ! over Gauss points)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    INTEGER :: npts, i, j, k, job, x
    REAL(kind=8) :: u, ux, uxx, uxxx, uxxxx, uxxxxx, uxxxxxx,&
         utau, p, add, weight, h, M, dummy, monalpha
    REAL(kind=8), PARAMETER :: wone = 0.08566224618958468d0,&
         wtwo = 0.18038078652407022d0,&
         wthree = 0.2339569672863451d0,&
         wfour = 0.23395696728634524d0,&
         wfive = 0.1803807865240697d0,&
         wsix = 0.08566224618958512d0
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: fmntr, prmtr
    npts = ipar(4)
    p = rpar(1)
    monalpha = rpar(1097)
    add = 0.0d0
    dummy = 0.0d0
    do i = 1,npts-1
       h = y(7*(i+1)) - y(7*i)
       fmntr(7*i) = 0.0d0
       do j = 1,6
          if (j == 1) then
             x = 1
             weight = wone
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          else if (j == 2) then
             x = 2
             weight = wtwo
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          else if (j == 3) then
             x = 3
             weight = wthree
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          else if (j == 4) then
             x = 4
             weight = wfour
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          else if (j == 5) then
             x = 5
             weight = wfive
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          else if (j == 6) then
             x = 6
             weight = wsix
             call derivatives(neq,i,x,y,yprime,u,ux,&
                  uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             call def_monitor(M,dummy,u,ux,uxx,rpar,ipar)
             fmntr(7*i)  = fmntr(7*i) + (1.0d0/6.0d0)*M
             add = add + h*weight*M
          end if
       end do
    end do
    fmntr(7*npts) = fmntr(7*(npts-1))
    prmtr = fmntr
    do k = 1,npts
       fmntr(7*k) = fmntr(7*k) + monalpha*add
    end do
  END SUBROUTINE monitor

  SUBROUTINE smooth_monitor(neq,fmntr,fsmooth,rpar,ipar)
    ! smooths the monitor function to reduce stiffness
    IMPLICIT NONE
    INTEGER :: npts, j, l, p
    INTEGER, INTENT(IN) :: neq
    INTEGER, DIMENSION(7) :: ipar
    REAL(kind=8) :: gamma, qgamma, divisor
    REAL(kind=8), DIMENSION(4) :: rpar
    REAL(kind=8), DIMENSION(*) :: fsmooth, fmntr
    npts = ipar(4)
    ! smoothing parameter
    gamma = 3.0d0
    qgamma = gamma/(1.0d0+gamma)
    ! smoothing index (currently only 1 and 2 are supported)
    p = 1
    do j = p,npts-p
       fsmooth(7*j) = 0.0d0
       divisor = 0.0d0
       do l = -p,p
          fsmooth(7*j) = fsmooth(7*j) + (fmntr(7*(j+l))**2.0d0)*(qgamma**(dble(abs(l))))
          divisor = divisor + (qgamma**(dble(abs(l))))
       end do
       fsmooth(7*j) = sqrt(fsmooth(7*j)/divisor)
    end do
    fsmooth(7) = sqrt(fmntr(7)**2.0d0 + fmntr(14)**2.0d0)
    fsmooth(7*npts) = sqrt(fmntr(7*(npts-1))**2.0d0 + &
         fmntr(7*npts)**2.0d0) 
    if (p == 2) then
       fsmooth(14) = sqrt(((fmntr(7)**2.0d0)*qgamma + &
            (fmntr(14)**2.0d0) + &
            (fmntr(21)**2.0d0/qgamma))/(2.0d0*qgamma)) 
       fsmooth(7*(npts-1)) = sqrt(((fmntr(7*(npts-2))**2.0d0)*qgamma + (fmntr(7*(npts-1))**2.0d0) + &
            (fmntr(7*npts))**2.0d0/qgamma)/(2.0d0*qgamma))
    end if
  END SUBROUTINE smooth_monitor

  SUBROUTINE def_ic(x,u0)
    IMPLICIT NONE
    REAL(kind=8), INTENT(IN) :: x
    REAL(kind=8), INTENT(OUT) :: u0
    REAL(kind=8), PARAMETER :: pi = 4.0d0*datan(1.0d0)

    u0 = (8.0d0/(dsqrt(2.0d0*pi)))*dexp(-x**2.0/2.0)
  END SUBROUTINE def_ic

  SUBROUTINE ic_fill(neq,y)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: y
    REAL(kind=8), DIMENSION(:), ALLOCATABLE :: coeff, xpart
    INTEGER :: ii, jj, kk, dord, pord, n, npts

    npts = (neq-1)/7
    pord = 4

    ! Left boundary
    ! do jj = 1,3
    !   dord = jj
    !   n = dord + pord
    !   ALLOCATE (coeff(1:n)) 
    !   ALLOCATE (xpart(1:n))
    !   do ii = 1,n
    !     xpart(ii) = y(7*ii)
    !   end do   

    !   call differ_stencil(y(7),dord,pord,xpart,coeff)
    !   do ii = 1,n
    !     y(3+jj) = y(3+jj) + coeff(ii)*(y(7*(ii+1)-4))
    !   end do
    !   DEALLOCATE (coeff)
    !   DEALLOCATE (xpart)
    ! end do

    ! Right boundary
    do jj = 1,3
      dord = jj
      n = dord + pord
      ALLOCATE (coeff(1:n)) 
      ALLOCATE (xpart(1:n))
      do ii = 1,n
        xpart(ii) = y(7*(npts-n+ii))
      end do   

      call differ_stencil(y(7*npts),dord,pord,xpart,coeff)
      do ii = 1,n
        y(7*npts-4+jj) = y(7*npts-4+jj) + coeff(ii)*(y(7*(npts-n+ii)-4))
      end do
      DEALLOCATE (coeff)
      DEALLOCATE (xpart)
    end do

    ! Right insert 1
    do jj = 1,3
      dord = jj
      n = dord + pord
      ALLOCATE (coeff(1:n)) 
      ALLOCATE (xpart(1:n))
      do ii = 1,n
        xpart(ii) = y(7*(npts-n+ii))
      end do   

      call differ_stencil(y(7*(npts-1)),dord,pord,xpart,coeff)
      do ii = 1,n
        y(7*(npts-1)-4+jj) = y(7*(npts-1)-4+jj) + coeff(ii)*(y(7*(npts-n+ii)-4))
      end do
      DEALLOCATE (coeff)
      DEALLOCATE (xpart)
    end do

    ! Right insert 2
    do jj = 1,3
      dord = jj
      n = dord + pord
      ALLOCATE (coeff(1:n)) 
      ALLOCATE (xpart(1:n))
      do ii = 1,n
        xpart(ii) = y(7*(npts-n+ii))
      end do   

      call differ_stencil(y(7*(npts-2)),dord,pord,xpart,coeff)
      do ii = 1,n
        y(7*(npts-2)-4+jj) = y(7*(npts-2)-4+jj) + coeff(ii)*(y(7*(npts-n+ii)-4))
      end do
      DEALLOCATE (coeff)
      DEALLOCATE (xpart)
    end do

    ! Right insert 3
    do jj = 0,3
      dord = jj
      n = dord + pord
      ALLOCATE (coeff(1:n)) 
      ALLOCATE (xpart(1:n))
      do ii = 1,n
        xpart(ii) = y(7*(npts-n+ii))
      end do   

      call differ_stencil(y(7*(npts-3)),dord,pord,xpart,coeff)
      do ii = 1,n
        y(7*(npts-3)-4+jj) = y(7*(npts-3)-4+jj) + coeff(ii)*(y(7*(npts-n+ii)-4))
      end do
      DEALLOCATE (coeff)
      DEALLOCATE (xpart)
    end do

    ! Right insert 4
    do jj = 1,3
      dord = jj
      n = dord + pord
      ALLOCATE (coeff(1:n)) 
      ALLOCATE (xpart(1:n))
      do ii = 1,n
        xpart(ii) = y(7*(npts-n+ii))
      end do   

      call differ_stencil(y(7*(npts-4)),dord,pord,xpart,coeff)
      do ii = 1,n
        y(7*(npts-4)-4+jj) = y(7*(npts-4)-4+jj) + coeff(ii)*(y(7*(npts-n+ii)-4))
      end do
      DEALLOCATE (coeff)
      DEALLOCATE (xpart)
    end do

    ! Mid region
    do kk = 1,npts-5
      do jj = 1,3
        dord = jj
        n = dord + pord
        ALLOCATE (coeff(1:n)) 
        ALLOCATE (xpart(1:n))
        do ii = 1,n
          if (kk-(n+1)/2+ii < 1) then
            xpart(ii) = -y(7*(kk+(n+1)/2-ii))
          else
            xpart(ii) = y(7*(kk-(n+1)/2+ii))
          end if
        end do  

        call differ_stencil(y(7*kk),dord,pord,xpart,coeff)
        do ii = 1,n
          if (kk-(n+1)/2+ii < 1) then
            y(7*kk+jj-4) = y(7*kk+jj-4) + coeff(ii)*(y(7*(kk+(n+1)/2-ii)-4))
          else
            y(7*kk+jj-4) = y(7*kk+jj-4) + coeff(ii)*(y(7*(kk-(n+1)/2+ii)-4))
          end if
        end do
        DEALLOCATE (coeff)
        DEALLOCATE (xpart)
      end do
    end do

  END SUBROUTINE ic_fill

  SUBROUTINE def_pde(g,u,ux,uxx,uxxx,uxxxx,&
       uxxxxx,uxxxxxx,rpar,ipar)
    IMPLICIT NONE
    REAL(kind=8), INTENT(OUT) :: g
    REAL(kind=8), INTENT(IN) :: u, ux, uxx, uxxx, uxxxx,&
         uxxxxx, uxxxxxx
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    REAL(kind=8), DIMENSION(*), INTENT(IN) :: rpar
    REAL(kind=8) :: p, delta
    INTEGER :: cons

    p = rpar(1)
    delta = 1.0d-3 ! Regularizing parameter
    cons = ipar(7)

    if (cons == 0) then
       g = ux*uxxxxx + u*uxxxxxx - 30*(u**4.0d0)*(ux**2.0d0) - &
               6*(u**5.0d0)*uxx
    elseif (cons == 1) then
       g = u*uxxxxx - 6.0d0*(u**5.0d0)*ux
    endif
  END SUBROUTINE def_pde

  SUBROUTINE def_monitor(M,x,u,ux,uxx,rpar,ipar)
    IMPLICIT NONE
    REAL(kind=8), INTENT(OUT) :: M
    REAL(kind=8), INTENT(IN) :: x, u, ux, uxx
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    REAL(kind=8), DIMENSION(*), INTENT(IN) :: rpar
    REAL(kind=8) :: p, nu, delta

    p = rpar(1)
    M = u**7.0d0 + abs((x**2.0d0)*uxx)**7.0d0
  END SUBROUTINE def_monitor


  SUBROUTINE solution_output(neq,y,yprime,rpar,ipar,mesh,soln)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    INTEGER :: i, j, npts, x
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(4+7*12*13), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(neq-6), INTENT(OUT) :: mesh,&
         soln
    REAL(kind=8) :: u, ux, uxx, uxxx, uxxxx, uxxxxx,&
         uxxxxxx, utau, h, coord
    REAL(kind=8), PARAMETER :: sone = 0.03376524289842475d0,&
         stwo = 0.16939530676686765d0,&
         sthree = 0.3806904069584014d0,&
         sfour = 0.6193095930415986d0,&
         sfive = 0.8306046932331324d0,&
         ssix = 0.9662347571015759d0

    npts = ipar(4)
    do i = 1, npts-1
       h = y(7*(i+1)) - y(7*i)
       mesh(7*i-6) = y(7*i)
       soln(7*i-6) = y(7*i-6)
       do j = 1,6
          if (j == 1) then
             x = 1
             coord = y(7*i) + sone*h
             mesh(7*i-5) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i-5) = u
          else if (j == 2) then
             x = 2
             coord = y(7*i) + stwo*h
             mesh(7*i-4) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i-4) = u
          else if (j == 3) then
             x = 3
             coord = y(7*i) + sthree*h
             mesh(7*i-3) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i-3) = u
          else if (j == 4) then
             x = 4
             coord = y(7*i) + sfour*h
             mesh(7*i-2) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i-2) = u
          else if (j == 5) then
             x = 5
             coord = y(7*i) + sfive*h
             mesh(7*i-1) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i-1) = u
          else if (j == 6) then
             x = 6
             coord = y(7*i) + ssix*h
             mesh(7*i) = coord
             call derivatives(neq,i,x,y,yprime,u,ux,uxx,uxxx,&
                  uxxxx,uxxxxx,uxxxxxx,utau,rpar)
             soln(7*i) = u
          end if
       end do
    end do

    mesh(neq-7) = y(7*npts)
    mesh(neq-6) = y(7*npts)
    soln(neq-7) = y(7*npts-6)
    soln(neq-6) = y(neq)
  END SUBROUTINE solution_output

  SUBROUTINE residual_ic(neq,tau,y,yprime,delta,ires,rpar,ipar)
    ! set up the dummy problem for generating an equidistributed mesh for 
    ! the initial conditions defined in def_ic
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: neq
    INTEGER, INTENT(INOUT) :: ires
    INTEGER, DIMENSION(*), INTENT(IN) :: ipar
    INTEGER :: i, j, npts
    REAL(kind=8), INTENT(IN) :: tau
    REAL(kind=8), DIMENSION(*), INTENT(IN) :: rpar
    REAL(kind=8), DIMENSION(neq), INTENT(IN) :: y, yprime
    REAL(kind=8), DIMENSION(neq), INTENT(INOUT) :: delta
    REAL(kind=8) :: u0
    REAL(kind=8) :: u, ux, uxx, uxxx, uxxxx, uxxxxx, uxxxxxx, utau, x, h
    REAL(kind=8), PARAMETER :: sone = 0.03376524289842475d0,&
         stwo = 0.16939530676686765d0,&
         sthree = 0.3806904069584014d0,&
         sfour = 0.6193095930415986d0,&
         sfive = 0.8306046932331324d0,&
         ssix = 0.9662347571015759d0
     REAL(kind=8), DIMENSION(6), PARAMETER :: points = &
         [sone, stwo, sthree, sfour, sfive, ssix]

    npts = ipar(4)

    ! put dummy equation in relevant res entries
    do i = 1,npts-1
       h = y(7*(i+1))-y(7*i)
       do j = 1,6

         x = y(7*i) + points(j)*h
         call derivatives(neq,i,j,y,yprime,u,ux,uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
         call def_ic(x,u0)
         delta(7*i+j-4+floor((dble(j-1))/3.0d0)) = utau-u0

       end do
    end do

    ! Fill in the two gaps on the left
    x = y(7)
    call derivatives(neq,1,7,y,yprime,u,ux,uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
    call def_ic(x,u0)
    delta(1)  = ux
    delta(2) = uxxx
    delta(3) = uxxxxx

    ! And the right
    x = y(7*npts)
    call derivatives(neq,npts-1,13,y,yprime,u,ux,uxx,uxxx,uxxxx,uxxxxx,uxxxxxx,utau,rpar)
    call def_ic(x,u0)
    delta(7*(npts-1)+4) = ux
    delta(7*(npts-1)+5) = uxxx
    delta(7*(npts-1)+6) = uxxxxx

    ! no time transformation in last entry
    delta(neq) = yprime(neq) - 1.0d0
  END SUBROUTINE residual_ic

END MODULE TFEmod
