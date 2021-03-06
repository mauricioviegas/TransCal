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
integer escmat,ierror,ntrans,opm,esc,m,impri ! Inteiros
common/geom/a,b,dx,dy                  ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3        ! Tamanho da malha
common/dados/cond,t1,t2,critconv       ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn           ! Calores das faces da aleta
allocatable :: x(:), y(:)                                !vetores posicao de x e y (Posi??o Tecplot)    
allocatable :: ae(:,:),aw(:,:),an(:,:),as(:,:),ap(:,:)   ! Coeficientes
allocatable :: t(:,:)                                    ! Campos de tempertatura


!Fechano arquivos localizados no mesmo lugar por seguranca
close(10)
close(12)
close(14)
close(15)
close(16)
close(17)
close(18)
close(20)

impri=1 !!Variavel para escrever no 10

!pegando valor da memoria para l1 e m1
open(15, file="tamanho.dat", action= "read")
read(15,*) l1, m1
close(15)
!Alocando matriz pontos
allocate (x(l1),y(m1),ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1),t(l1,m1))


!Escrevendo nome do programa
write(*,'(/ "----- PROGRAMA ALETA 2D EM REGIME PERMANENTE ----" )')
write(*,*) ""     


!Do para que sempre volte para o menu
do
        
	!Pedindo dados para usuario
    write(*,*) "**************************"
    write(*,*) "     ----- MENU -----"
    write(*,*) ""
    write(*,*) ""
    write(*,*) "Digite a opcao desejada:"
    write(*,*) ""
    write(*,*) "1. Configurar dimensoes da aleta"
	write(*,*) "2. Tamanho da malha"
	write(*,*) "3. Material da aleta"
	write(*,*) "4. Temperaturas das paredes"
	write(*,*) "5. Criterio de convergencia"
	write(*,*) "6. Rodar programa"
    write(*,*) "7. Dependencia de malha"
    write(*,*) "8. Instrucoes"
    write(*,*) "9. Sair"
    write(*,*) ""
    write(*,*) "***************************"
    write(*,*) ""
    read(*,*) opm

    
	if (opm==1) then

    !Pedindo as dimensoes da aleta para usuario 
	write(*,*) "Digite o comprimento da aleta em m."
	write(*,*) "Para melhor precisao utilize o formato ""[tamanho].d0""."
	read(*,*) a
	write(*,*) ""
	write(*,*) "Digite o espessura da aleta em m."
	write(*,*) "Para melhor precisao utilize o formato ""[tamanho].d0""."
	read(*,*) b
	write(*,*) ""

	!Gravando os dados no arquivo memoria
	open(14, file="dimensoes.dat", action= "write")
	write(14,*) a, b
	close(14)
	
    !a=.05.D0    ! Comprimento da aleta = 5 cm
    !b=.01.D0	! Espessura da aleta = 1 cm 

	elseif (opm==2) then

    !Pedindo quantidade de pontos
	write(*,*) "Digite o numero de volumes na horizintal. (numero inteiro)"
	read(*,*) l1
	write(*,*) ""
	write(*,*) "Digite o numero de volumes na vertical. (numero inteiro)"
	read(*,*) m1
	write(*,*) ""

    !Desalocando matriz de pontos para nao dar erro
	deallocate (x, y ,ae, aw, an, as, ap, t)
	!Alocando matriz pontos
    allocate (x(l1),y(m1),ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1),t(l1,m1))

	!Gravando os dados no arquivo memoria
	open(15, file="tamanho.dat", action= "write")
	write(15,*) l1, m1
	close(15)
	
	
	elseif (opm==3) then

    !Pedindo tipo de material para usuario
	do
	    escmat=0    !Para nao marcar nenhuma opcao antes
		write(*,*) "Informe o material da aleta:"
    	write(*,*) ""
    	write(*,*) "1 para ferro fundido"
    	write(*,*) "2 para aluminio"
		write(*,*) "3 para outros"
    	read(*,*) escmat
		write(*,*) ""

		!Verificando escolha do usuario
		if (escmat==1) then        !Caso escolha ferro fundido
	    	cond=72.D0             ! Ferro fundido k = 72 W/m.K
			exit
		elseif (escmat==2) then    !Caso escolha aluminio
	    	cond=237.D0            ! Aluminio k = 237 W/m.K
			exit
		elseif (escmat==3) then    !Caso escolha outros materiais
		    write(*,*) "Digite a condutividade do material [W/m.K]."
			write(*,*) "Para melhor precisao utilize o formato ""[condutividade].d0""."
    	    read(*,*) cond
		    write(*,*) ""
			exit
		else    !Caso nao escolha umas das opcoes
	    	write(*,*) "Escolha uma opcao valida!"
			write(*,*) ""
		endif
	enddo

	!Gravando dado no arquivo memoria
	open(16, file="material.dat", action= "write")
	write(16,*) cond
	close(16)
	

	elseif (opm==4) then

    !Declarando a temperatura da aleta
	write(*,*) "Digite a temperatura do lado esquerdo da aleta em graus Celsius."
	write(*,*) "Para melhor precisao utilize o formato ""[temperatura].d0""."
	read(*,*) t1
	write(*,*) ""
	write(*,*) "Digite a temperatura de cima da aleta."
	write(*,*) "Para melhor precisao utilize o formato ""[temperatura].d0""."
	read(*,*) t2
	write(*,*) ""

	!Gravando os dados no arquivo memoria
	open(17, file="condcont.dat", action= "write")
	write(17,*) t1, t2
	close(17)
	    
	!t1=  400.D0       ! Temperatura da esquedo
    !t2=  20.D0        ! Temperatura de cima

	
	elseif (opm==5) then
    
	!Declarando o criterio de convencia
    write(*,*) "Digite o criterio de convergencia no formato ""1.d-[casas desejadas]""."
	read(*,*) critconv
	write(*,*) ""

	!Gravando dado no arquivo memoria
	open(18, file="critconv.dat", action= "write")
	write(18,*) critconv
	close(18)
	
	!critconv=1.d-8    ! Crit?rio de converg?ncia

	elseif (opm==6) then

    
	!Pegando dados da memoria
    open(14, file="dimensoes.dat", action= "read")
	read(14,*) a, b
	close(14)
	open(16, file="material.dat", action= "read")
	read(16,*) cond
	close(16)
	open(17, file="condcont.dat", action= "read")
	read(17,*) t1, t2
	close(17)
	open(18, file="critconv.dat", action= "read")
	read(18,*) critconv
	close(18)


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
    call malha (x,y,ae,aw,an,as,ap,t,impri)
    call coefcc (x,y,ae,aw,an,as,ap,t)
    call tincc (x,y,ae,aw,an,as,ap,t)
    call gauss (x,y,ae,aw,an,as,ap,t,impri)
    call fluxos (x,y,ae,aw,an,as,ap,t)
    call prints (x,y,ae,aw,an,as,ap,t)
        
    
	!Escrevendo mensagem de finaliza??o
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
	
	
	elseif (opm==7) then
	    do
	    
		!Pedindo confirmacao de continuacao para usuario
		write(*,*) "Este processo pode ser EXTREMAMENTE longo. Tem certeza que deseja continuar?"
		write(*,*) "0. Sim"    !0 para realmente so apertar se tiver certeza
		write(*,*) "1. Nao"
		read(*,*) esc
        
		!conferindo escolha
		if (esc==0) then

		    !Pegando dados da memoria
            open(14, file="dimensoes.dat", action= "read")
            read(14,*) a, b
            close(14)
            open(16, file="material.dat", action= "read")
            read(16,*) cond
            close(16)
            open(17, file="condcont.dat", action= "read")
            read(17,*) t1, t2
            close(17)
            open(18, file="critconv.dat", action= "read")
            read(18,*) critconv
            close(18)

		    impri=0 !Variavel para nao escrever no 10

		    !Variavel zerada para usar como indice do arquivo gravado
            ierror=0
            ntrans=48           !48 significa zero em caracter
            n= char (ntrans)    !Convertendo 0 em caracter

    
            !Abrindo arquivos para colocar resultados
            open(20, file="Conv_Malha "//n//".txt", status= "new", action= "write", iostat= ierror)

    
            !Verificando de o arquivo ja existe

            do
                if (ierror>0) then
                    close (20)
                   ntrans=ntrans+1
                   n= char (ntrans)    !Convertendo inteiro em caracter
                   open  (20, file= "Conv_Malha "//n//".txt", status= "new", action= "write", iostat= ierror)
                else
	               exit
                endif
            enddo

            !Pedindo quantidade de pontos
            write(*,*) "Digite o numero de volumes inicial na horizintal. (numero inteiro)"
            read(*,*) l1
            write(*,*) ""
            write(*,*) "Digite o numero de volumes inicial na vertical. (numero inteiro)"
            read(*,*) m1
            write(*,*) ""
            write(*,*) "Digite o incremento de numero de volumes na horizintal. (numero inteiro)"
            read(*,*) incr1
            write(*,*) ""
            write(*,*) "Digite o incremento de numero de volumes na vertical. (numero inteiro)"
            read(*,*) incr2
            write(*,*) ""

            !Desalocando matriz de pontos para nao dar erro
            deallocate (x, y ,ae, aw, an, as, ap, t)
            !Alocando matriz pontos
            allocate (x(l1),y(m1),ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1),t(l1,m1))
		    
			
			!Inicializando contagem do tempo
            tempo_s=timef()       !Funcao para contar segundos
            tempo_h=clock()       !Funcao para exibir relogio
            write(*,*) tempo_h    !Exibindo hora do comeco do processamento na tela
            write(*,*) ""
			
			
			write (*,*) "Calculando..."
			write (*,*) ""
            
            m=0    !contador de malhas feitas
				
			!Fazer ate convergir
			do
			
			    m=m+1
			    
                !Chamar as subrotinas
                call malha (x,y,ae,aw,an,as,ap,t,impri)
				WRITE(*,*) "Malha gerada."
				call coefcc (x,y,ae,aw,an,as,ap,t)
				WRITE(*,*) "Coeficientes gerados."
                call tincc (x,y,ae,aw,an,as,ap,t)
				WRITE(*,*) "Temperatura gerada"
                call gauss (x,y,ae,aw,an,as,ap,t,impri)
				WRITE(*,*) "Gauss Seidel executado."
				call fluxos (x,y,ae,aw,an,as,ap,t)
				WRITE(*,*) "Fluxo gerado."
                
			    if (calorw>=calorn) then
				    erro=dabs(calorw-calorn)/calorw   !dabs - absoluto para real de dupla precisao
  	            else
				    erro=dabs(calorn-calorw)/calorn   !dabs - absoluto para real de dupla precisao
				endif
					            
				!Escreve mensagem de acopanhamento o usuario
	            write(*,*) "Valores da malha ", m
	            write (*, '(2I8,2F15.6)') l1, m1, calorw, calorn
	            write (20, '(2I8,2F15.6)') l1, m1, calorw, calorn
	            write(*,*) ""

				! Testa erro para ver se a malha jah convergiu
	            if (erro<1.d-5 .and. m>2) exit
	
				!Somando incrementos para proxima malha
				l1=l1+incr1
	            m1=m1+incr2

				

	            !Desalocando matriz de pontos para nao dar erro
	            deallocate (x, y ,ae, aw, an, as, ap, t)
	            !Alocando matriz pontos
	            allocate (x(l1),y(m1),ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1),t(l1,m1))

			    
	        enddo
			
            	
			write(*,*) "O PROGRAMA FINALIZOU!"
            write(*,*) "Os resultados estao no arquivo Conv_Malha ", n, ".txt."
            write(*,*) ""

			!Finalizando contagem do tempo
            tempo_s=timef()       !Funcao para contar segundos
            tempo_h=clock()       !Funcao para exibir relogio
            TEMPO_M=TEMPO_S/60.d0
            WRITE(20,'(/"TEMPO DE PROCESSAMENTO=" f13.4 " s = " f10.3 " min")')TEMPO_S,TEMPO_M
            write(*,*) tempo_h    !Exibindo hora do final do processamento na tela
        	write(*,*) ""
            
            close(20)
			impri=1

			exit

		elseif (esc==1) then
		    exit

		else
		    write(*,*) "Escolha uma opcao valida!"
		    write(*,*) ""
		
		endif
		  
		enddo 



					
	elseif (opm==8) then

        !Escrevendo a funcao do programa
        write (*,*) "    Este programa foi desenvolvido para calcular o campo de temperaturas e a  "
        write (*,*) "quantidade de calor de uma aleta retangular isolada no seu lado direito e     "
        write (*,*) "inferior. Como mostra o desenho abaixo:"
		write (*,*) ""
		write (*,*) ""
		write (*,*) "                                       T2"
		write (*,*) "                             ______________________"
		write (*,*) "                            |                      |\"
		write (*,*) "                         T1 |                      |\"
		write (*,*) "                            |                      |\"
		write (*,*) "                            |______________________|\"
		write (*,*) "                             \\\\\\\\\\\\\\\\\\\\\\\\"
		write (*,*) ""
		write (*,*) ""
		write (*,*) "    Para o programa funcionar sera necessario o fornecimento das dimensoes da "
		write (*,*) "aleta, tamanho da malha, condutividade do material da aleta, temperatura nas  "
		write (*,*) "paredes esquerda e superior e o criterio de convergencia."
		write (*,*) ""
		write (*,*) "    Os resultados, apos os calculos, serao colocados em dois arquivos com o   "
		write (*,*) "nome Temp_Aleta2D N.txt e Temp_Tecplot N.txt. O programa suporta ate 20 arquivos"
		write (*,*) "na mesma pasta do programa."
        write (*,*) ""
		write (*,*) "OBS.: Cuidado com malhas pequenas, pois pode gerar instabilidade no software. "
		write (*,*) "Caso isso aconte?a digite CTRL+C."
		write (*,*) ""
		write (*,*) "                                                                    29/10/2010"

		!Para voltar ao menu
		do while (i==0)
		    write(*,*)"Digite um numero para voltar ao menu."
			write(*,*)""
			read(*,*) i
		enddo

    elseif (opm==9) then
	    exit

	else 	   
		!Teste caso o usuario entregue uma opcao nao valida
		write (*,*) "Escolha uma opcao valida."
		write (*,*) ""
		
	endif

enddo


end program aleta




!************************************************************************     
subroutine malha (x,y,ae,aw,an,as,ap,t,impri)
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm, impri                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura

l2=l1-1      
l3=l1-2
m2=m1-1
m3=m1-2
            
dx=a/l3    ! Malha igualmente espa?ada
dy=b/m3      

!Nao imprime na opcao 7
if (impri==1) then
    write(10,'(/ "dx=" f10.6 "    dy=" f10.6 /)') dx, dy
endif

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
subroutine coefcc (x,y,ae,aw,an,as,ap,t) !Coeficientes e condi??es de contorno
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura


! Gen?rico para malha igualmente espa?ada
do i=2,l2
    do j=2,m2
        ae(i,j)=cond*dy/dx    ! Leste
        aw(i,j)=ae(i,j)       ! Oeste
        an(i,j)=cond*dx/dy    ! Norte
        as(i,j)=an(i,j)       ! Sul
    enddo
enddo


do i=2,l2	! Condi??es de contorno (fluxo prescrito)
  an(i,m2)=2.d0*an(i,m2)
  as(i,2)= 2.d0*as(i,m2)	 
enddo

! Condi??es de contorno em X 
do i=2,l2
    an(i,m2)=2.d0*an(i,m2)    ! Condicao de contorno 1 especie temperatura prescrita
    as(i,2)= 2.d0*as(i,m2)
enddo

! Condi??es de contorno em Y
do j=2, m2
    aw(2,j)=2.d0*aw(2,j)    ! Condicao de contorno 1 especie temperatura prescrita
    ae(l2,j)= 2.d0*ae(l2,j)
enddo

do i=2,l2
    do j=2,m2
        ap(i,j)=ae(i,j)+aw(i,j)+an(i,j)+as(i,j)-SP(i,j)    ! Equa??o para calcular Ap
    enddo
enddo

endsubroutine coefcc




!************************************************************************      
subroutine tincc (x,y,ae,aw,an,as,ap,t) !Temperatura inicial e condi??es de contorno
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura


! Arbitragem inicial
tm=(t1+t2)/2.d0
do i=2,l2
    do j=2,m2
        t(i,j)=tm
    enddo
enddo

DO I=2,L2
 DO J=2,M2 !todos
  SC(I,J)=0
 ENDDO
ENDDO

DO J=2,M2
 SC(1,J)=qfront/dx   !fluxo
ENDDO

!DO I=L1,L2   ! C?lculo de Q, o calor gerado fonte 1
 !DO J=2,M2
  !SC(I,J)=qd*dx*dy
 !ENDDO
!ENDDO

DO I=2,L2    
 DO J=2,M2
  SP(I,J)=0
 ENDDO
ENDDO
       
DO I=2,L2    ! fluxo e temp
  SC(I,M1)=SC(I,M1)+T2/(dx*(1/h1+dx/(2.d0*cond)))
  SP(I,M1)=SP(I,M1)-1/(dx*(1/h1+dx/(2.d0*cond)))
  SC(I,1)=SC(I,1)+T2/(dx*(1/h2+dx/(2.d0*cond)))
  SP(I,1)=SP(I,1)-1/(dx*(1/h2+dx/(2.d0*cond)))
ENDDO

DO I=2,L1   !todos 
  SC(I,1)=SC(I,1)+T2/(dx*(1/h+dx/(2*cond)))
  SP(I,1)=SP(I,1)-1/(dx*(1/h+dx/(2*cond)))
ENDDO

DO J=2,M2
  SC(L1,J)=SC(L1,J)+T2/(dy*(1/h2+dy/(2.d0*cond)))
  SP(L1,J)=SP(L1,J)-1/(dY*(1/h2+dY/(2.d0*cond)))
ENDDO

!DO J=1,M1 !temp prescritra 
  !T(1,J)=T1
!ENDDO
       
endsubroutine tincc      




!************************************************************************      
subroutine gauss (x,y,ae,aw,an,as,ap,t,impri) !Seidel
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm,impri                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura
      
!Nao imprime na opcao 7
if (impri==1) then
    write(10,'(3x "ITER" 8x "T(L2,2)" 8x "T(2,M2)")')
endif

! Rotina valida para problemas lineares
lstop=.TRUE.      ! Dizendo lstop e verdadeiro ja q o default e falso
iter=0

do while(lstop)	  
    tc1=t(l2,2)	  ! Temperatura de Conferencia 1
    tc2=t(2,m2)	  ! Temperatura de Conferencia 2
    iter=iter+1 
	

     DO I=2,L2
  DO J=2,M2
   T(I,J)=(ae(I,J)*T(I+1,J)+aw(I,J)*T(I-1,J)+an(I,J)*T(I,J+1)+as(I,J)*T(I,J-1)+ SC(I,J))/ap(I,J)
  ENDDO
 ENDDO

DO I=2,L2
   T(I,M1)=(2.d0*cond*T(I,M2)/dy+h1*T2)/(2.d0*cond/dy+h1)
   T(I,1)=(2.d0*cond*T(I,2)/dy+h2*T2)/(2.d0*cond/dy+h2)
 ENDDO

DO J=2,M2
   T(L1,J)=(2.d0*cond*T(L2,J)/dx+h2*T2)/(2.d0*cond/dx+h2) ! leste
      T(1,J)=qfront*dx/(2.d0*cond)+T(2,J) ! fluxo
 ENDDO

    erro1=dabs(tc1-t(l2,2))/t(l2,2)    !dabs - absoluto para real de dupla precisao
    erro2=dabs(tc2-t(2,m2))/t(2,m2)
    write (*, '(I8,2F15.6)') iter,tc1,tc2
    !Nao imprime na opcao 7
    if (impri==1) then
        write (*, '(I8,2F15.6)') iter,tc1,tc2
        write (10,'(I8,2F15.6)' ) iter,tc1,tc2
    endif

    ! Testa os 2 erros para ver se ja convergiu com mais de 200 iteracoes
	if (erro1<critconv .and. erro2<critconv .and. iter>200) lstop=.FALSE. 

enddo

! Definindo as temperaturas nas faces isoladas      
do i=1,l1		
    t(i,1)=t(i,2)
enddo
do j=1,m1		
    t(l1,j)=t(l2,j)
enddo

endsubroutine gauss     




!************************************************************************      
subroutine fluxos (x,y,ae,aw,an,as,ap,t)
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura

!Zerando variaveis para calculos
calorw=0.d0
calorn=0.d0

DO J=2,M2
  !CALORW=CALORW+cond*dy*(T(1,J)-T(2,J))/(dx/2.d0)    ! CONVEN??O DE TRANSCAL
  CALORW=qfront*b    !fluxo
  CALORE=CALORE+cond*dy*(T(L1,J)-T(L2,J))/(dx/2.d0)  
ENDDO
DO I=2,L2
  CALORN=CALORN+cond*dx*(T(I,M1)-T(I,M2))/(dy/2.d0)
  CALORS=CALORS+cond*dx*(T(I,1)-T(I,2))/(dy/2.d0)
ENDDO

QGER=qd*b*a*.1d0

BAL=CALORW+CALORN+CALORE+CALORS+QGER

endsubroutine fluxos




!************************************************************************      
subroutine prints (x,y,ae,aw,an,as,ap,t)
!------------------------------------------------------------------------

!Declarando variaveis
implicit double precision (a-h,o-z)    ! Reais de dupla precisao
logical lstop
character(1) n                         ! Caracter do numero
character(8) tempo_h                   ! Caracter de tempo
integer escmat, ierror, ntrans, opm                           ! Inteiros
common/geom/a,b,dx,dy                                         ! Geometria
common/pontos/l1,l2,l3,m1,m2,m3                               ! Tamanho da malha
common/dados/cond,t1,t2,critconv                              ! Constantes e crit?rio de convergencia
common/calores/calorw,calorn                                  ! Calores das faces da aleta
real*8 :: x(l1), y(m1)                                        !vetores posicao de x e y (Posi??o Tecplot)    
real*8 :: ae(l1,m1),aw(l1,m1),an(l1,m1),as(l1,m1),ap(l1,m1)   ! Coeficientes
real*8 :: t(l1,m1)                                            ! Campos de tempertatura



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
WRITE(10,'(/ "CALOR OESTE=" F15.6 " W    CALOR NORTE=" f15.6 " W")')CALORW,CALORN  
WRITE(10,'(/ "CALOR LESTE=" F15.6 " W    CALOR SUL=" f15.6 " W")')CALORE,CALORS
WRITE(10,'(/ "CALOR GERADO=" F15.6 " W")')QGER
WRITE(10,'(/ "BALAN?O    =" F15.6 " W")')BAL

! Escrevendo arquivo do campo de Temperaturas para o TECPLOT
write(12,'("Variables = x, y, T")')
write(12,'("Zone T = " "Zone-One" " I=" I3 " J=" I3 " F=Point")') l1,m1

do j=1,m1
    do i=1,l1
	    write(12,'(3f20.10)') x(i),y(j),t(i,j)
    enddo
enddo
      
endsubroutine prints          



