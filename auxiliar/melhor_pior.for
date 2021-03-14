c	Para extrair do arquivo imediato o melhor e o pior modelo quando o programa
c	ficar preso. Melhor e pior ja trocados

	real melhor(1000), pior(1000), permz(700)
	integer bloco(1400), cont, n_l, n_prop, ativos
	
	

	open(1,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\ACTIVE_BLOCKS.dat',
     1status='unknown')
	
	open(2,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\IMEDIATO.DAT',
     1status='unknown')
	
	open(3,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\POROMELHOR.DAT',
     1status='unknown')

	open(4,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\PERMXMELHOR.DAT',
     1status='unknown')

	open(5,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\
     1POROPIOR.DAT',status='unknown')

	open(6,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\
     1PERMXPIOR.DAT',status='unknown')

	open(7,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\
     1PERMZMELHOR.DAT',status='unknown')
	
	open(8,file='D:\flavio\mestrado\resultados_flavio\fase3\
	1caixa60_lambda10_inicialmedia_8211it\
     1PERMZPIOR.DAT',status='unknown')

	n_l=700
	n_prop=2	
	ativos=488

	cont=1
	
	do i=1, 1400
		read(1,*) bloco(i)
	enddo
	close(1)
	
	read(2,*)
	read(2,*)
	
	do i=1,n_prop*ativos 	
		read(2,*)  pior(i), melhor(i)
	enddo
	
	close(2)
	
	
	do i=1, n_l
	
		if(bloco(i).EQ.1)then
				
				write(3,*) melhor(cont)
				write(5,*) pior(cont)
								
			
c				write(4,*) melhor(cont)
c				write(6,*) pior(cont)
c				write(7,*) 0.31*melhor(cont) + 3.12 !permz
c				write(8,*) 0.31*pior(cont) + 3.12 !permz
c			endif
			
			cont=cont+1
		else
			write(3,*) 0.0
			write(5,*) 0.0
	
		endif
	enddo

	do i=701, n_prop*n_l
		
		if(bloco(i).EQ.1)then
			
				write(4,*) melhor(cont)
				write(6,*) pior(cont)
				write(7,*) 0.31*melhor(cont) + 3.12 !permz
				write(8,*) 0.31*pior(cont) + 3.12 !permz
					
			cont=cont+1
		else

			write(4,*) 0.0
			write(6,*) 0.0
			write(7,*) 0.0
			write(8,*) 0.0
		endif
	enddo
	
	close(3)
	close(4)
	close(5)
	close(6)
	close(7)
	close(8)

	stop
	end