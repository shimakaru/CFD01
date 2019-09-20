PROGRAM each_kakezan
	IMPLICIT NONE
	INTEGER(8) :: min_num,max_num,number,hantei,kake,temp_num,temp_hantei
	INTEGER :: num(100)
	INTEGER :: i,j
	OPEN(1, file='output.dat', status='replace')
	
	min_num = 0
	max_num = 999999999999999999

	number = min_num
	DO while (number < max_num)
		temp_hantei = hantei(number)
		IF (temp_hantei > 0) THEN
			temp_num = number
			DO i = 1,20
				temp_num = kake(temp_num)
				IF (temp_num < 10) THEN
					EXIT
				END IF
			END DO		
			PRINT *,number,'/',max_num,' have been calculated!'
			IF ( i > 10) THEN
				OPEN(1, file='output.dat', position='append')
				write (1,*) number,i,temp_num
				PRINT *,number,i,temp_num
			END IF
		END IF
		!PRINT *,number,i,temp_num,temp_hantei
		number = number + ABS(temp_hantei)
	END DO
	close(1)
	PRINT *,'Fin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
END PROGRAM each_kakezan

!---------------------------------------------------------------------
!「各桁の数字を掛け合わす」という操作１回分を行う関数。
!---------------------------------------------------------------------
INTEGER(8) FUNCTION kake( n )
	IMPLICIT NONE
	INTEGER(8) :: n,temp_n
	INTEGER :: num(100)
	INTEGER :: i

	num(1) = MOD(n,10)
	temp_n = num(1)
	DO i = 2,100
		IF ( INT(n/10**(i-1)) == 0 ) THEN
		EXIT
		END IF
	num(i) = INT(MOD(n,10**i)/10**(i-1))
	temp_n = temp_n * num(i)
	END DO
	kake = temp_n
END FUNCTION kake

!---------------------------------------------------------------------
!まず計算する必要がある数字であるかどうかを判断。「５と偶数、１、これより小さい数字の組み合わせ」がある場合は-1を出す。
!次に、１桁目に９がある場合、次に計算すべき数字までの差を算出。これを上で出した±１にかける。
!つまり、この関数で出力された値は、計算すべき数字かどうかは整数の正負で判定、かつその絶対値は次に計算すべき値との差を意味する。
!---------------------------------------------------------------------
INTEGER FUNCTION hantei( n )
	IMPLICIT NONE
	INTEGER(8) :: n,temp_n
	INTEGER :: num(100),temp(100)
	INTEGER :: i,j,k

	hantei = 1
	num(1) = MOD(n,10)
	DO i = 2,100
		num(i) = INT(MOD(n,10**i)/10**(i-1))
		IF ( INT(n/10**(i-1)) == 0 ) THEN
		EXIT
		END IF
		IF (num(i) == 1) THEN
			hantei = -1 !1が含まれる場合は-1を返す。１は掛け算では無視できるため、必ずそれより小さい同じ表現の組み合わせがある。
			EXIT
		END IF
		DO j = 2,i
			IF (MOD(num(i) * num(i-j+1),10) == 0) THEN
				hantei = -1 !偶数と5が存在する場合は-1を返す。
				EXIT
				EXIT
			ELSE IF (num(i) > num(i-j+1)) THEN
				hantei = -1 !同じ順列の数列がこれより小さい数字で存在する場合は-1を返す。
				EXIT
				EXIT
			END IF
		END DO
	END DO
	temp_n = 1
	IF (num(1) == 9) THEN
		DO j = 2,100
			IF (num(j) /= 9) THEN
				temp(j) = num(j)
				EXIT
			END IF
		END DO
		DO k = 1,j-1
			temp_n = temp_n + (temp(j) + 1)*10**(k - 1) 
		END DO
	END IF
	hantei = hantei * temp_n
END FUNCTION hantei