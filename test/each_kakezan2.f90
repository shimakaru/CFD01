PROGRAM each_kakezan
	IMPLICIT NONE
	INTEGER(8) :: max_num,number,hantei,kake,temp_num
	INTEGER :: num(100),temp(100)
	INTEGER :: i,j,k,l,m
	OPEN(1, file='output.dat', status='replace')
	
	max_num = 999999999999999999
	DO number = 277777788888898,277777788888899
		IF (hantei(number) /= 0) THEN
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
				PRINT *,number,i,temp_num,hantei(number)
			END IF
		END IF
	END DO
	close(1)
END PROGRAM each_kakezan

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

INTEGER FUNCTION hantei( n )
	IMPLICIT NONE
	INTEGER(8) :: n,temp_n
	INTEGER :: num(100)
	INTEGER :: i,j

	hantei = 1
	num(1) = MOD(n,10)
	DO i = 2,100
		IF ( INT(n/10**(i-1)) == 0 ) THEN
		EXIT
		END IF
	num(i) = INT(MOD(n,10**i)/10**(i-1))
	IF (num(i) == 1) THEN
		hantei = 0 !1が含まれる場合は0を返す。１は掛け算では無視できるため、必ずそれより小さい同じ表現の組み合わせがある。
		EXIT
	END IF
		DO j = 2,i
			IF (MOD(num(i) * num(i-j+1),10) == 0) THEN
			hantei = 0 !偶数と5が存在する場合は0を返す。
			EXIT
			EXIT
			ELSE IF (num(i) > num(i-j+1)) THEN
			hantei = 0 !同じ順列の数列がこれより小さい数字で存在する場合は0を返す。
			EXIT
			EXIT
			END IF
		END DO
	END DO
END FUNCTION hantei