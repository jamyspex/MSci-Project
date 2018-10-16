module sub
     use module_init
     use module_dyn
     use module_shapiro
     interface init
       module procedure init
     end interface init
     interface dyn
       module procedure dyn
     end interface dyn
     interface shapiro
       module procedure shapiro
     end interface shapiro
end module sub
