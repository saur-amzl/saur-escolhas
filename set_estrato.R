# Criando a tabela de referência com UF, estrato e descrição
tabela_estratos_2008 <- data.frame(
  COD_UF = c(
    rep(11, 22),  # Rondônia
    rep(12, 8),   # Acre
    rep(13, 26),  # Amazonas
    rep(14, 8),   # Roraima
    rep(15, 32),  # Pará
    rep(16, 12),   # Amapá
    rep(17, 20),  # Tocantins
    rep(21, 48),  # Maranhão
    rep(51, 36)   # Mato Grosso
  ),
  ESTRATO_POF = c(
    # Rondônia
    1:11,
    # Rondônia -- todo Estado
    1:11,
    # Acre
    1:4,
    # Acre -- todo Estado
    1:4,    
    # Amazonas
    1:13,
    # Amazonas  -- todo Estado
    1:13,    
    # Roraima
    1:4,
    # Roraima -- todo Estado
    1:4,
    # Pará
    1:14,
    # Pará -- todo Estado
    1:14,
    # Pará -- Capital e RRM
    1:4,
    # Amapá
    1:6,
    # Amapá -- todo Estado
    1:6,
    # Tocantins
    1:10,
    # Tocantins -- todo Estado
    1:10,
    # Maranhão
    1:24,
    # Maranhão -- todo Estado
    1:24,
    # Mato Grosso
    1:18,
    # Mato Grosso -- todo Estado
    1:18
  ),
  descricao_estrato = c(
    # Rondônia
    rep("RO-UCapital", 2), 
    rep("RO-URUF", 4), 
    rep("RO-Rural", 5),
    rep("RO-UF", 11),
    # Acre
    "AC-UCapital", 
    "AC-URUF", 
    rep("AC-Rural", 2),
    rep("AC-UF", 4),
    # Amazonas
    rep("AM-UCapital", 4), 
    rep("AM-URUF", 4), 
    rep("AM-Rural", 5),
    rep("AM-UF", 13),
    # Roraima
    "RR-UCapital", 
    "RR-URUF", 
    rep("RR-Rural", 2),
    rep("RR-UF", 4),    
    # Pará
    rep("PA-UCapital", 2), 
    rep("PA-URRM", 2), 
    rep("PA-URUF", 4), 
    rep("PA-Rural", 6),
    rep("PA-UF",14),
    rep("PA-CapitalRRM",4),
    # Amapá
    "AP-UCapital", 
    rep("AP-URUF", 2), 
    rep("AP-Rural", 3),
    rep("AP-UF",6),    
    # Tocantins
    "TO-UCapital", 
    rep("TO-URUF", 4), 
    rep("TO-Rural", 5),
    rep("TO-UF",10),    
    # Maranhão
    rep("MA-UCapital", 3), 
    rep("MA-URUF", 9), 
    rep("MA-Rural", 12),
    rep("MA-UF", 24),    
    # Mato Grosso
    rep("MT-UCapital", 3), 
    rep("MT-URUF", 7), 
    rep("MT-Rural", 8),
    rep("MT-UF", 18)
  ),
  stringsAsFactors = FALSE
)


# Criando a tabela de referência com UF, estrato e descrição para 2018
tabela_estratos_2018 <- data.frame(
  UF = c(
    rep(11, 22),  # Rondônia
    rep(12, 8),   # Acre
    rep(13, 39),  # Amazonas
    rep(14, 10),   # Roraima
    rep(15, 43),  # Pará
    rep(16, 17),   # Amapá
    rep(17, 16),   # Tocantins
    rep(21, 54),  # Maranhão
    rep(51, 39)   # Mato Grosso
  ),
  ESTRATO_POF = c(
    # Rondônia
    1101:1111,
    # Rondônia -- Estado
    1101:1111,
    # Acre
    1201:1204,
    # Acre -- Estado
    1201:1204,
    # Amazonas
    1301:1316,
    # Amazonas -- Estado
    1301:1316,
    # Amazonas -- CapitalRRM
    1301:1307,
    # Roraima
    1401:1405,
    # Roraima -- Estado
    1401:1405,
    # Pará
    1501:1519,
    # Pará -- Estado
    1501:1519,
    # Pará -- CapitalRRM
    1501:1505,
    # Amapá
    1601:1607,
    # Amapá -- Estado
    1601:1607,
    # Amapá -- CapitalRRM
    1601:1603,
    # Tocantins
    1701:1708,
    # Tocantins -- Estado
    1701:1708,
    # Maranhão
    2101:2125,
    # Maranhão -- Estado
    2101:2125,
    # Maranhão -- CapitalRRM
    2101:2104,    
    # Mato Grosso
    5101:5118,
    # Mato Grosso -- Estado
    5101:5118,
    # Mato Grosso -- CapitalRRM
    5101:5103
  ),
  descricao_estrato = c(
    # Rondônia
    rep("RO-UCapital", 2), 
    rep("RO-URUF", 5), 
    rep("RO-Rural", 4),
    rep("RO-UF", 11),
    # Acre
    "AC-UCapital", 
    "AC-URUF", 
    rep("AC-Rural", 2),
    rep("AC-UF", 4),
    # Amazonas
    rep("AM-UCapital", 6), 
    "AM-URRM", 
    rep("AM-URUF", 3), 
    rep("AM-Rural", 6),
    rep("AM-UF",16),
    rep("AM-CapitalRRM",7),
    # Roraima
    rep("RR-UCapital", 2), 
    "RR-URUF", 
    rep("RR-Rural", 2),
    rep("RR-UF", 5),    
    # Pará
    rep("PA-UCapital", 3), 
    rep("PA-URRM", 2), 
    rep("PA-URUF", 6), 
    rep("PA-Rural", 8),
    rep("PA-UF", 19),
    rep("PA-CapitalRRM",5),
    # Amapá
    rep("AP-UCapital", 2), 
    "AP-URRM", 
    "AP-URUF", 
    rep("AP-Rural", 3),
    rep("AP-UF", 7),
    rep("AP-CapitalRRM",3),
    # Tocantins
    "TO-UCapital", 
    rep("TO-URUF", 4), 
    rep("TO-Rural", 3),
    rep("TO-UF", 8),
    # Maranhão
    rep("MA-UCapital", 3), 
    "MA-URRM", 
    rep("MA-URUF", 9), 
    rep("MA-Rural", 12),
    rep("MA-UF", 25),
    rep("MA-CapitalRRM",4),
    # Mato Grosso
    rep("MT-UCapital", 2), 
    "MT-URRM", 
    rep("MT-URUF", 9), 
    rep("MT-Rural", 6),
    rep("MT-UF", 18),
    rep("MT-CapitalRRM",3)
  ),
  stringsAsFactors = FALSE
)

