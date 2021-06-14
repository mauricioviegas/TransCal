!************************************************************************
!                ALETA 2D , REGIME PERMANENTE
!************************************************************************     
program ALETA


!Programa de Analise de uma aleta 2d em regime permanente


!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
allocatable :: x(:), y(:)                                !vetores posicao de x e y (Posição Tecplot)    
allocatable :: ae(:,:),aw(:,:),an(:,:),as(:,:),ap(:,:)   ! Coeficientes
allocatable :: t(:,:), SP(:,:), SC(:,:)                  ! Campos de tempertatura


!Tamanho de malha
l1=100
m1=20

!Alocando matriz pontos
allocate (x(l1),y(m1),ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1),t(l1,m1), SP(l1,m1), SC(l1,m1) )

!Escrevendo nome do programa
write(*,'(/ "----- PROGRAMA ALETA 2D EM REGIME PERMANENTE ----" )')
write(*,*) ""     

!dimensoes da aleta 
a=0.05d0   ! Comprimento da aleta = 5 cm 
b=0.01d0   ! Espessura da aleta = 1 cm

!tipo de material
!cond=72.D0             ! Ferro fundido k = 72 W/m.K
cond=237.D0            ! Aluminio k = 237 W/m.K

!Declarando a temperatura da aleta
t1=400.d0        ! Temperatura da esquedo
t2=20.d0         ! Temperatura de cima e baixo

!Declarando o criterio de convencia
critconv=1.d-8    ! Critério de convergência

!Declarando Termo Fonte	
termf=1.d-50

!Definindo 3 especie
!h1=500.d0 !h=500W/m^2*K
!h2=500.d0 !h=500W/m^2*K
h2=1.d-50 !fluxo nulo
h1=1.d50  !temperatura prescrita




	!Variavel zerada para usar como indice do arquivo gravado
    ierror=0
    ntrans=48           !48 significa zero em caracter
    n= char (ntrans)    !Convertendo 0 em caracter

    
	!Abrindo arquivos para colocar resultados
    open(10, file="Temp_Aleta2D "//n//".txt", status= "new", action= "write", iostat= ierror)

    
	!Verificando de o arquivo ja existe
    do
        if (ierror>0) then
            close (10)
            ntrans=ntrans+1
            n= char (ntrans)    !Convertendo inteiro em caracter
            open  (10, file= "Temp_Aleta2D "//n//".txt", status= "new", action= "write", iostat= ierror)
        else
	        exit
        endif
    enddo

    write(10,'(2x "  --------- ALETA BIDIMENSIONAL --------- ")')    !Primeira linha do arquivo
	write(10,*) ""    !Primeira linha do arquivo

    
	!Arquivo para plotagem
    open (12, file= "Temp_Tecplot "//n//".txt", status= "new", action= "write")


    !Inicializando contagem do tempo
    tempo_s=timef()       !Funcao para contar segundos
    tempo_h=clock()       !Funcao para exibir relogio
    write(*,*) tempo_h    !Exibindo hora do comeco do processamento na tela
    write(*,*) ""

    !Chamar as subrotinas
    call malha (x,y,ae,aw,an,as,ap,t,SP,SC)
    call coefcc (x,y,ae,aw,an,as,ap,t,SP,SC)
    call tincc (x,y,ae,aw,an,as,ap,t,SP,SC)
    call gauss (x,y,ae,aw,an,as,ap,t,SP,SC)
    call fluxos (x,y,ae,aw,an,as,ap,t,SP,SC)
    call prints (x,y,ae,aw,an,as,ap,t,SP,SC)
        
    
	!Escrevendo mensagem de finalização
	write(*,*) "O PROGRAMA FINALIZOU!"
	write(*,*) "Os resultados estao no arquivo Temp_Aleta2D ", n, ".txt."
	write(*,*) "O arquivo de plotagem e Temp_Tecplot ", n, ".txt."
	write(*,*) ""

	!Finalizando contagem do tempo
	tempo_s=timef()       !Funcao para contar segundos
    tempo_h=clock()       !Funcao para exibir relogio
    TEMPO_M=TEMPO_S/60.d0
    WRITE(10,'(/"TEMPO DE PROCESSAMENTO=" f13.4 " s = " f10.3 " min")')TEMPO_S,TEMPO_M
	write(*, "(A,f13.4,A)") "Tempo de processamento: ", tempo_s, " seg."
    write(*,*) tempo_h    !Exibindo hora do final do processamento na tela
	write(*,*) ""

	!Fechamento arquivos de resultados
    close(10)   
    close(12)
	

end program aleta




!************************************************************************     
subroutine malha (x,y,ae,aw,an,as,ap,t,SP,SC)
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                                 ! Campos de tempertatura

l2=l1-1      
l3=l1-2
m2=m1-1
m3=m1-2
            
dx=a/l3    ! Malha igualmente espaçada
dy=b/m3      


    write(10,'(/ "dx=" f10.6 "    dy=" f10.6 /)') dx, dy


x(1)=0.d0  ! Calculo das posicoes x de uma malha igualmente espacada
x(2)=dx/2.d0
do i=3,l2
  x(i)=x(i-1)+dx
enddo
x(l1)=a

y(1)=0.d0  ! Calculo das posicoes y de uma malha igualmente espacada
y(2)=dy/2.d0
do i=3,m2
  y(i)=y(i-1)+dy
enddo
y(m1)=b


endsubroutine malha




!************************************************************************      
subroutine coefcc (x,y,ae,aw,an,as,ap,t,SP,SC) !Coeficientes e condições de contorno
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                                            ! Campos de tempertatura


! Genérico para malha igualmente espaçada
do i=2,l2
    do j=2,m2
        ae(i,j)=cond*dy/dx    ! Leste
        aw(i,j)=ae(i,j)       ! Oeste
        an(i,j)=cond*dx/dy    ! Norte
        as(i,j)=an(i,j)       ! Sul
    enddo
enddo


! Condições de contorno em X

    do i=2,l2
        an(i,m2)=2.d0*an(i,m2)    ! Condicao de contorno 1 especie temperatura prescrita
        as(i,2)= 2.d0*as(i,m2)     ! Condicao de contorno 1 especie temperatura prescrita
    enddo
 
! Condições de contorno em Y

    do j=2,m2
        ae(l2,j)= 0.d0	        ! Fluxo igual a zero
       aw(2,j)=2.d0*aw(2,J)    ! Condicao de contorno 1 especie temperatura prescrita
    enddo




DO I=1, L1   
    DO J=1,M1
        SP(I,J)=1.d-50*dx*dy*1.d0 !zero em too o volume
    ENDDO
ENDDO

! Cálculo de SP para 3 especie
!Calculo de cima e de baixo da aleta
DO I=2,L2    
  SP(I,M2)=SP(I,M1)+1/(dx*(1/h1+dx/(2.d0*cond)))*dx*dy*1.d0
  SP(I,2)=SP(I,1)+1/(dx*(1/h2+dx/(2.d0*cond)))*dx*dy*1.d0
ENDDO

do i=2,l2
    do j=2,m2
        ap(i,j)=ae(i,j)+aw(i,j)+an(i,j)+as(i,j)+SP(i,j)    ! Equação para calcular Ap
    enddo
enddo

endsubroutine coefcc




!************************************************************************      
subroutine tincc (x,y,ae,aw,an,as,ap,t,SP,SC) !Temperatura inicial e condições de contorno
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                 ! Campos de tempertatura


! Arbitragem inicial
tm=(t1+t2)/2.d0
do i=2,l2
    do j=2,m2
        t(i,j)=tm
    enddo
enddo

! Condicao de contorno de cima
do i=1,l1
    t(i,m1)=t2
	t(i,1)=t2
enddo

! Condicao de contorno do lado esquerdo da aleta
do j=1,m1
    t(1,j)=t1
enddo

! Cálculo de Termo Fonte zerando termo fonte em todos os volumes
DO I=1, L1   
    DO J=1,M1
        SC(I,J)=termf*dx*dy*1.d0 !volume
    ENDDO
ENDDO


! Cálculo de SC para 3 especie
!Calculo de cima e de baixo da aleta
DO I=2,L2    ! fluxo e temp
  SC(I,M2)=SC(I,M2)+t2/(dx*(1/h1+dx/(2.d0*cond)))*dx*dy*1.d0
  SC(I,2)=SC(I,2)+t2/(dx*(1/h2+dx/(2.d0*cond)))*dx*dy*1.d0
ENDDO
       
endsubroutine tincc      




!************************************************************************      
subroutine gauss (x,y,ae,aw,an,as,ap,t,SP,SC) !Seidel
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                                 ! Campos de tempertatura
      

    write(10,'(3x "ITER" 8x "T(L2,2)" 8x "T(2,M2)")')


! Rotina valida para problemas lineares
lstop=.TRUE.      ! Dizendo lstop e verdadeiro ja q o default e falso
iter=0

do while(lstop)	  
    tc1=t(l2,2)	  ! Temperatura de Conferencia 1
    tc2=t(2,m2)	  ! Temperatura de Conferencia 2
    iter=iter+1 
	

    !Calculando Gauss Seidel
	do i=2,l2
        do j=2,m2
            t(i,j)=(ae(i,j)*t(i+1,j)+aw(i,j)*t(i-1,j)+an(i,j)*t(i,j+1)+as(i,j)*t(i,j-1)+SC(i,j))/ap(i,j)
        enddo
    enddo

    !Calculo de Temperatura de superficie par 3 especie
    !Para cima e baixo da aleta
	DO I=2,L2
        T(I,M1)=(2.d0*cond*T(I,M2)/dy+h1*t2)/(2.d0*cond/dy+h1)
		T(I,1)=(2.d0*cond*T(I,2)/dy+h2*t2)/(2.d0*cond/dy+h2)
    ENDDO


    erro1=dabs(tc1-t(l2,2))/t(l2,2)    !dabs - absoluto para real de dupla precisao
    erro2=dabs(tc2-t(2,m2))/t(2,m2)
       
        
        write (*, '(I8,2F15.6)') iter,tc1,tc2
        write (10,'(I8,2F15.6)' ) iter,tc1,tc2
    

    ! Testa os 2 erros para ver se ja convergiu com mais de 200 iteracoes
	if (erro1<critconv .and. erro2<critconv .and. iter>200) lstop=.FALSE. 

enddo

!Definindo as temperaturas nas faces isoladas      

do j=1,m1		
    t(l1,j)=t(l2,j)
enddo

endsubroutine gauss     




!************************************************************************      
subroutine fluxos (x,y,ae,aw,an,as,ap,t,SP,SC)
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn,CALORS,BAL,QGER           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                                 ! Campos de tempertatura

!Zerando variaveis para calculos
calorw=0.d0
calorn=0.d0
CALORS=0.d0

!Calculando fluxo no lado esquerdo da aleta
do j=2,m2
    calorw=calorw+cond*dy*(t(1,j)-t(2,j))/(dx/2.d0)    ! Equação para conducao bidimensional em regime permanente
enddo


!Calculando fluxo em cima da aleta
do i=2,l2
    calorn=calorn+cond*dx*(t(i,m1)-t(i,m2))/(dy/2.d0)    ! Equação para conducao bidimensional em regime permanente
enddo


DO I=2,L2
  CALORS=CALORS+cond*dx*(T(I,1)-T(I,2))/(dy/2.d0)
ENDDO


QGER=termf*b*a*1.d0

BAL=CALORW+CALORN+QGER


endsubroutine fluxos




!************************************************************************      
subroutine prints (x,y,ae,aw,an,as,ap,t,SP,SC)
!------------------------------------------------------------------------

!Declarando variaveis
use dfport                             ! Biblioteca para o tempo
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop                          ! Logico 
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer ierror,ntrans ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv,termf,h1,h2 ! Constantes e critério de convergencia
common/calores/calorw,calorn,CALORS,BAL,QGER           ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posição Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1), SP(l1,m1), SC(l1,m1)                                 ! Campos de tempertatura



! Impresao do campo de temperaturas
write(10,'(/3X"-------- CAMPO DE TEMPERATURAS --------")')

!Dando valores a indices
iend=0
ifst=1
jfst=1
ibeg=ifst-7

!Escrevendo um bloco de temperaturas
do while (iend<l1)
    ibeg=ibeg+7
    iend=ibeg+6
    iend=min0(iend,l1)
    
	!Escrevendo numero do i e a letra j para coluna
	write(10,'(/)')    ! Pula uma linha - outro jeito   
    write(10,'(3X "I=", I4, 6I11)') (i,i=ibeg,iend)
    write(10,'(1X,"J")')

    !Escrevendo linha por linha as temperaturas de um bloco de temperaturas
    jfl=jfst+m1
    do jj=jfst,m1
        j=jfl-jj
        write(10,'(I2,1P7D11.3)') j,(t(i,j),i=ibeg,iend)
    enddo
enddo


!Escrevendo valor do fluxo de calor
write(10,'(/ "CALOR OESTE=" F15.6 " W    CALOR NORTE=" f15.6 " W")') calorw, calorn  
WRITE(10,'(/ "CALOR SUL=" f15.6 " W")'),CALORS
WRITE(10,'(/ "CALOR GERADO=" F15.6 " W")')QGER
WRITE(10,'(/ "BALANÇO    =" F15.6 " W")')BAL

! Escrevendo arquivo do campo de Temperaturas para o TECPLOT
write(12,'("Variables = x, y, T")')
write(12,'("Zone T = " "Zone-One" " I=" I3 " J=" I3 " F=Point")') l1,m1

do j=1,m1
    do i=1,l1
	    write(12,'(3f20.10)') x(i),y(j),t(i,j)
    enddo
enddo
      
endsubroutine prints          



