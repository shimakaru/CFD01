PROGRAM each_kakezan
	IMPLICIT NONE
	INTEGER(8) :: min_num1,max_num1,number1,temp_hantei1,temp_num1,temp_kake1
	INTEGER(8) :: min_num2,max_num2,number2,temp_hantei2,temp_num2,temp_kake2
	INTEGER(8) :: hantei,kake,extra
	INTEGER :: i,j
	OPEN(1, file='output.dat', status='replace')
	
	min_num1 = 0
	max_num1 = 999999999999999999 !999999999999999999 !最大「9223372036854775807」までいけるが、仕様上９のゾロ目じゃないとバグる。
	min_num2 = 0
	max_num2 = 999999999999999999 !こっちは変数の型におさまれば何でもいい。

	number1 = min_num1
	number2 = min_num2
	DO WHILE (number2 < max_num2 + 1)
		DO WHILE (number1 < max_num1 + 1)
			temp_hantei1 = hantei(number1)
			IF (number2 == 0) THEN
				temp_hantei2 = 1
			ELSE
				temp_hantei2 = hantei(number2)
			END IF
			IF (temp_hantei1 > 0 .and. temp_hantei2 > 0) THEN
				temp_num1 = number1
				temp_num2 = number2
				DO i = 1,20
					temp_kake1 = kake(temp_num1)
						IF (temp_num2 == 0) THEN
							temp_kake2 = 1
						ELSE
							temp_kake2 = kake(temp_num2)
						END IF
					temp_num1 = MOD((temp_kake1 * temp_kake2),(max_num1 + 1))
					temp_num2 = INT((temp_kake1 * temp_kake2)/(max_num1 + 1))
					IF (temp_num1 < 10 .and. temp_num2 == 0) THEN
						EXIT
					END IF
				END DO		
				!PRINT *,number,'/',max_num,' have been calculated!'
				IF ( i > 10) THEN
					OPEN(1, file='output.dat', position='append')
					write (1,*) number2,number1,i,temp_num1,extra(number1)
					PRINT *,number2,number1,i,temp_num1
					EXIT
					EXIT
				END IF
			END IF
			number1 = number1 + ABS(temp_hantei1)
		END DO
	number2 = number2 + ABS(temp_hantei2)
	number1 = max_num1 * MOD(number2 , 10) / 9 !「9999・・・」まで行ったら次は「1111・・・」
	END DO
	close(1)
	PRINT *,'Fin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
END PROGRAM each_kakezan

!---------------------------------------------------------------------
!まず計算する必要がある数字であるかどうかを判断。「５と偶数、１、これより小さい数字の組み合わせ」がある場合は-1を出す。
!次に、１桁目に９がある場合、次に計算すべき数字までの差を算出。これを上で出した±１にかける。
!つまり、この関数で出力された値は、計算すべき数字かどうかは整数の正負で判定、かつその絶対値は次に計算すべき値との差を意味する。
!---------------------------------------------------------------------
INTEGER(8) FUNCTION hantei( n )
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

INTEGER(8) FUNCTION extra(n)
	IMPLICIT NONE
	INTEGER(8) :: n,temp_num
	INTEGER :: sosu,sosu_num(10),max_sosu_num
	INTEGER :: i

	temp_num = n
	max_sosu_num = 100
	sosu = 2
	DO WHILE (sosu < 10)
		DO i = 0,max_sosu_num
			if (MOD(temp_num , sosu) == 0)THEN
				temp_num = temp_num / sosu
			ELSE
				EXIT
			END IF
		END DO
		SELECT CASE(sosu)
			CASE(2)
				sosu = 3
			CASE(3)
				sosu = 5
			CASE(5)
				sosu = 7
			CASE(7)
				sosu = 10
			CASE DEFAULT
				PRINT *,'バグったかも'
		END SELECT
	END DO
	IF (temp_num < 10) THEN
		PRINT *,'まだ上があるで！'
		extra = -1
	END IF
END FUNCTION extra