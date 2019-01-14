module module_timdata
 contains
      subroutine timdata()
      use common_sn 
      open(50,file='winddata.dat')
      return
      end subroutine timdata
end module module_timdata
