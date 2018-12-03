module Platform (Platform(..)) where

data Platform = CPU | MIC | GPU | FPGA deriving (Eq,Show,Read)  
