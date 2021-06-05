
      FUNCTION URAND()  ! return random number U(0,1) 
*     (simple generator, showing principle)
      PARAMETER (IA=205,IC=29573,IM=139968)
      DATA LAST/4711/
      LAST=MOD(IA*LAST+IC,IM)
      IF(LAST.EQ.0) LAST=MOD(IA*LAST+IC,IM)
      URAND=FLOAT(LAST)/FLOAT(IM) 
      END 

      FUNCTION JRAND(JA,JB) ! return random integer from [JA,JB]
      PARAMETER (IA=205,IC=29573,IM=139968)
      DATA LAST/4711/
      LAST=MOD(IA*LAST+IC,IM)
      JRAND=JA+((JB-JA+1)*LAST)/IM
      END 

      FUNCTION MCPOI(XMU) ! return random integer 0 ...
*     from Poisson distribution with mean XMU
*     (for large XMU slow,  use normal approximation) 
      DATA ARG/0.0/
      SAVE EXPM,ARG
      IF(XMU.NE.ARG) THEN
         ARG =XMU  ! mean value    
         EXPM=EXP(-ARG)
      END IF
      K=0      
      A=1.0
   10 U=RANU()   ! U(0,1) random number
      A=A*U
      IF(A.GE.EXPM) THEN
         K=K+1
         GOTO 10
      END IF 
      MCPOI=K ! K follows Poisson distribution
      END 

      SUBROUTINE RANSHI(N,A)
*     return N random numbers U(0,1) in array A(N) 
      PARAMETER (NB=511)
      PARAMETER (IA=16807,IM=2147483647,IQ=127773,IR=2836)   
      PARAMETER (AEPS=1.0E-10,SCALIN=4.6566125E-10)
      COMMON/RANBUF/MBUFF(0:NB),IAN,IC,IBOOST
      REAL A(*)
      INTEGER ISTART
      IROTOR(M,N)=IEOR(IOR(ISHFT(M,17),ISHFT(M,-15)),N)
      DATA ISTART/0/,IWARM/10/,ISEED/4711/
      IF(ISTART.NE.0) GOTO 20 
      WRITE(*,*) ' Automatic RANSHI initialization using:'
*     initialize buffer
 10   IDUM=ISEED+9876543          ! prevent damage, if iseed=0
      WRITE(*,*) '           ISEED=',ISEED,'   IWARM=',IWARM      
      DO J=0,NB+1                 ! fill buffer
       K=IDUM/IQ                  ! minimal standard generator
       IDUM=IA*(IDUM-K*IQ)-IR*K   !    with Schrages method
       IF(IDUM.LT.0) IDUM=IDUM+IM !
       MBUFF(J)=ISHFT(IDUM,1)     ! fill in leading bit 
      END DO 
      IAN=IAND(IAN,NB)            ! mask angle
      IC=1                        ! set pointer
      IBOOST=0
      DO J=1,IWARM*NB             ! warm up a few times
       IT=MBUFF(IAN)              ! hit ball angle
       MBUFF(IAN)=IROTOR(IT,IC)   ! new spin 
       IC=IT                      ! replace red spin
       IAN=IAND(IT+IBOOST,NB)     ! boost and mask angle
       IBOOST=IBOOST+1            ! increment boost
      END DO 
      IF(ISTART.LT.0) RETURN      ! return for RBNVIN
      ISTART=1                    ! set done-flag 
*     generate array of r.n.
 20   DO I=1,N
       IT=MBUFF(IAN)              ! hit ball angle
       MBUFF(IAN)=IROTOR(IT,IC)   ! new spin
       IC=IT                      ! replace red spin
       IAN=IAND(IT+IBOOST,NB)     ! boost and mask angle
       A(I)=FLOAT(ISHFT(IT,-1))*SCALIN+AEPS ! avoid zero output
       IBOOST=IBOOST+1            ! increment boost
      END DO
      IBOOST=IAND(IBOOST,NB) 
      RETURN
      ENTRY RANVIN(JSEED,JWARM)   ! initialize, but only once 
      IF(ISTART.EQ.0) THEN 
         WRITE(*,*) ' RANSHI initialization by RANVIN-call using:'
         ISEED=JSEED              ! copy seed and  
         IWARM=JWARM              !    warm-up parameter
         ISTART=-1                ! start flag 
         GOTO 10
      END IF
      END     

      FUNCTION RANU()
*     random number U(0,1) using RANSHI
      PARAMETER (NDIM=100)
      REAL BUFFER(NDIM)
      DATA INDEX/NDIM/
      SAVE INDEX,BUFFER
      INDEX=MOD(INDEX,NDIM)+1
      IF(INDEX.EQ.1) CALL RANSHI(NDIM,BUFFER)
      RANU=BUFFER(INDEX)
      END  


      FUNCTION MCBIN(N,P) ! return random integer 0 ... N 
*     from Binomial distribution (N,p) 
      PP=MAX(P,1.0-P) ! P is probability
      K=0
      DO I=1,N        ! N is number of trials
       IF(RANU().LT.PP) K=K+1  
      END DO 
      MCBIN=K
      IF(P.LT.0.5) MCBIN=N-K
      END


