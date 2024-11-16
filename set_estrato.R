# Criando a tabela de referência com UF, estrato e descrição
tabela_estratos_2008 <- data.frame(
  COD_UF = c(
    rep(11, 11),  # Rondônia
    rep(12, 4),   # Acre
    rep(13, 13),  # Amazonas
    rep(14, 4),   # Roraima
    rep(15, 14),  # Pará
    rep(16, 6),   # Amapá
    rep(17, 10),  # Tocantins
    rep(21, 24),  # Maranhão
    rep(51, 18)   # Mato Grosso
  ),
  ESTRATO_POF = c(
    # Rondônia
    1:11,
    # Acre
    1:4,
    # Amazonas
    1:13,
    # Roraima
    1:4,
    # Pará
    1:14,
    # Amapá
    1:6,
    # Tocantins
    1:10,
    # Maranhão
    1:24,
    # Mato Grosso
    1:18
  ),
  descricao_estrato = c(
    # Rondônia
    rep("RO-UCapital", 2), 
    rep("RO-URUF", 4), 
    rep("RO-Rural", 5),
    # Acre
    "AC-UCapital", 
    "AC-URUF", 
    rep("AC-Rural", 2),
    # Amazonas
    rep("AM-UCapital", 4), 
    rep("AM-URUF", 4), 
    rep("AM-Rural", 5),
    # Roraima
    "RR-UCapital", 
    "RR-URUF", 
    rep("RR-Rural", 2),
    # Pará
    rep("PA-UCapital", 2), 
    rep("PA-URRM", 2), 
    rep("PA-URUF", 4), 
    rep("PA-Rural", 6),
    # Amapá
    "AP-UCapital", 
    rep("AP-URUF", 2), 
    rep("AP-Rural", 3),
    # Tocantins
    "UCapital", 
    rep("TO-URUF", 4), 
    rep("TO-Rural", 5),
    # Maranhão
    rep("MA-UCapital", 3), 
    rep("MA-URUF", 9), 
    rep("MA-Rural", 12),
    # Mato Grosso
    rep("MT-UCapital", 3), 
    rep("MT-URUF", 7), 
    rep("MT-Rural", 8)
  ),
  stringsAsFactors = FALSE
)


# Criando a tabela de referência com UF, estrato e descrição para 2018
tabela_estratos_2018 <- data.frame(
  UF = c(
    rep(11, 11),  # Rondônia
    rep(12, 4),   # Acre
    rep(13, 16),  # Amazonas
    rep(14, 5),   # Roraima
    rep(15, 19),  # Pará
    rep(16, 7),   # Amapá
    rep(17, 8),   # Tocantins
    rep(21, 25),  # Maranhão
    rep(51, 18)   # Mato Grosso
  ),
  ESTRATO_POF = c(
    # Rondônia
    1101:1111,
    # Acre
    1201:1204,
    # Amazonas
    1301:1316,
    # Roraima
    1401:1405,
    # Pará
    1501:1519,
    # Amapá
    1601:1607,
    # Tocantins
    1701:1708,
    # Maranhão
    2101:2125,
    # Mato Grosso
    5101:5118
  ),
  descricao_estrato = c(
    # Rondônia
    rep("RO-UCapital", 2), 
    rep("RO-URUF", 5), 
    rep("RO-Rural", 4),
    # Acre
    "AC-UCapital", 
    "AC-URUF", 
    rep("AC-Rural", 2),
    # Amazonas
    rep("AM-UCapital", 6), 
    "AM-URRM", 
    rep("AM-URUF", 3), 
    rep("AM-Rural", 6),
    # Roraima
    rep("RR-UCapital", 2), 
    "RR-URUF", 
    rep("RR-Rural", 2),
    # Pará
    rep("PA-UCapital", 3), 
    rep("PA-URRM", 2), 
    rep("PA-URUF", 6), 
    rep("PA-Rural", 8),
    # Amapá
    rep("AP-UCapital", 2), 
    "AP-URRM", 
    "AP-URUF", 
    rep("AP-Rural", 3),
    # Tocantins
    "TO-UCapital", 
    rep("TO-URUF", 4), 
    rep("TO-Rural", 3),
    # Maranhão
    rep("MA-UCapital", 3), 
    "MA-URRM", 
    rep("MA-URUF", 9), 
    rep("MA-Rural", 12),
    # Mato Grosso
    rep("MT-UCapital", 2), 
    "MT-URRM", 
    rep("MT-URUF", 9), 
    rep("MT-Rural", 6)
  ),
  stringsAsFactors = FALSE
)

