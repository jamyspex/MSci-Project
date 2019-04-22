! Relevant array declaration
real, dimension(0:302,0:302,0:81) :: p0

! Before stencil constant removal
! Loop nest has reverse ordering
do j = 0, 301, 1
   do i = 0, 301, 1
      p0(i,j,0) = p0(i,j,1)
      p0(i,j,81) = p0(i,j,80)
   end do
end do

! After loop index insert and constant removal
do si0 = 0, 81, 1
   do j = 0, 301, 1
      do i = 0, 301, 1
         p0(i,j,si0) = p0(i,j,si0+1)
         p0(i,j,si0+81) = p0(i,j,si0+80)
      end do
   end do
end do

! Final result
! After LHS offset removal + guard insertion
do si0 = 0, 81, 1
   do j = 0, 301, 1
      do i = 0, 301, 1
         if (si0==0) then
            p0(i,j,si0) = p0(i,j,si0+1)
         end if
         if (si0==81) then
            p0(i,j,si0) = p0(i,j,si0-1)
         end if
       end do
   end do
end do