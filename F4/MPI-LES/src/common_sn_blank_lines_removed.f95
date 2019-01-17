module common_sn
    use params_common_sn 
    implicit real*4(a-h,o-z)
    implicit integer(i-n)
    real a1(1:ip,1:jp+1,1:kp+1),a2(1:ip,1:jp+1,1:kp+1) ,a3(1:ip,1:jp+1,1:kp+1)
    integer irec
   character(len=70) :: filename
end module common_sn
