program hello
implicit none
integer(8) :: number,temp_num
integer :: sosu,sosu_num(10),max_sosu_num
integer :: i

number = 344444444467777779
temp_num = number
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
PRINT *,temp_num
	IF (temp_num < 10) THEN
	PRINT *,'まだ上があるで！'
	END IF
end program Hello