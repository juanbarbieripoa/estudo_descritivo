# ===================================================================
# 20_gerar_fichas_carteiras.R
# Projeto: estudo_descritivo — UEF-SMED-PMPA
# ===================================================================
# Produz 11 fichas gerenciais das carteiras operacionais de 2026.
# Universo: 53 escolas elegíveis; 11 assessoras; excluídas ESC_001,
# ESC_055 e ESC_056. Resultados educacionais não compõem o índice.
# Não produz ranking, posição, faixa, percentil ou cenários alternativos.
# ===================================================================

library(here)
library(tidyverse)

id_execucao <- format(Sys.time(), "%Y%m%d_%H%M%S")
branch_esperada <- "refatoracao_modulo_21"
commit_base <- "08909963e394556d32711ceba66d18b80664efff"
TENTAR_GERAR_PDF_COMPILADO <- TRUE

script_canonico <- here("R", "20_gerar_fichas_carteiras.R")
ids_excluidos <- c("ESC_001", "ESC_055", "ESC_056")

manifestos <- c(
  modulo_18 = here("documentacao", "analise_carteiras", "execucao_20260729_221336", "18_manifesto_produtos_modulo_18.csv"),
  modulo_19 = here("documentacao", "fichas_escolas", "execucao_20260729_233939", "16_manifesto_produtos_modulo_19.csv")
)

entradas <- c(
  analise_csv = here("dados_finais", "analise_carteiras_assessoras.csv"),
  analise_rds = here("dados_finais", "analise_carteiras_assessoras.rds"),
  detalhe_csv = here("dados_finais", "carteira_escola_detalhe.csv"),
  detalhe_rds = here("dados_finais", "carteira_escola_detalhe.rds"),
  composicao_csv = here("dados_finais", "composicao_operacional_carteiras.csv"),
  composicao_rds = here("dados_finais", "composicao_operacional_carteiras.rds"),
  diagnostico_csv = here("dados_finais", "diagnostico_educacional_carteiras.csv"),
  diagnostico_rds = here("dados_finais", "diagnostico_educacional_carteiras.rds"),
  universo_csv = here("dados_finais", "universo_institucional_carteiras.csv"),
  universo_rds = here("dados_finais", "universo_institucional_carteiras.rds"),
  fichas_escolas_csv = here("dados_finais", "base_fichas_escolas.csv"),
  fichas_escolas_rds = here("dados_finais", "base_fichas_escolas.rds"),
  indice_escolas_csv = here("dados_finais", "indice_fichas_escolas.csv"),
  indice_escolas_rds = here("dados_finais", "indice_fichas_escolas.rds")
)

saidas <- c(
  base_csv = here("dados_finais", "base_fichas_carteiras.csv"),
  base_rds = here("dados_finais", "base_fichas_carteiras.rds"),
  indice_csv = here("dados_finais", "indice_fichas_carteiras.csv"),
  indice_rds = here("dados_finais", "indice_fichas_carteiras.rds"),
  dicionario_csv = here("documentacao", "fichas_carteiras", "dicionario_base_fichas_carteiras.csv")
)

pasta_doc <- here("documentacao", "fichas_carteiras")
pasta_execucao <- file.path(pasta_doc, paste0("execucao_", id_execucao))
pasta_resultados <- here("resultados", "fichas_carteiras")
pasta_final <- file.path(pasta_resultados, paste0("execucao_", id_execucao))
pasta_transacao <- file.path(pasta_resultados, "transacoes", paste0("execucao_", id_execucao))
pasta_candidatos <- file.path(pasta_transacao, "candidatos")
pasta_html <- file.path(pasta_candidatos, "html")
pasta_graficos <- file.path(pasta_candidatos, "graficos")
pasta_rollback <- file.path(pasta_transacao, "rollback")
pasta_historico <- here("dados_finais", "historico", "fichas_carteiras", paste0("pre_refatoracao_execucao_", id_execucao))
walk(c(pasta_doc, pasta_execucao, pasta_resultados, pasta_transacao, pasta_candidatos, pasta_html, pasta_graficos, pasta_rollback, pasta_historico), ~dir.create(.x, recursive=TRUE, showWarnings=FALSE))

md5 <- function(x) if (file.exists(x)) unname(tools::md5sum(x)) else NA_character_
norm <- function(x, must=TRUE) normalizePath(x, winslash="/", mustWork=must)
git_out <- function(args) {
  z <- tryCatch(system2("git", args, stdout=TRUE, stderr=FALSE), error=function(e) character())
  if (!is.null(attr(z,"status")) && attr(z,"status") != 0) return(character())
  stringr::str_squish(as.character(z))
}
git_head <- function() {
  z <- git_out(c("-C", shQuote(here()), "rev-parse", "HEAD")); z <- z[str_detect(z,"^[0-9a-fA-F]{40}$")]
  if (length(z)==1) str_to_lower(z) else NA_character_
}
git_branch <- function() {
  z <- git_out(c("-C", shQuote(here()), "branch", "--show-current")); if(length(z)==1) z else NA_character_
}
tipo <- function(x) case_when(inherits(x,"Date")~"date", inherits(x,"POSIXct")~"datetime", is.logical(x)~"logical", is.integer(x)~"integer", is.double(x)~"double", is.character(x)~"character", TRUE~paste(class(x),collapse="|"))
logico <- function(x,nm) {
  if(is.logical(x)) return(x)
  z <- str_to_lower(str_squish(as.character(x)))
  y <- case_when(is.na(z)|z==""~NA, z%in%c("true","t","1","sim","s")~TRUE, z%in%c("false","f","0","nao","não","n")~FALSE, TRUE~NA)
  if(any(!is.na(z)&z!=""&is.na(y))) stop("Valor lógico inválido em ",nm)
  y
}
converter <- function(x,m,nm) switch(tipo(m), character=as.character(x), logical=logico(x,nm), integer=suppressWarnings(as.integer(x)), double=suppressWarnings(as.double(x)), date=as.Date(x), datetime=as.POSIXct(x,tz="UTC"), stop("Tipo não suportado: ",nm))
ler_csv_modelo <- function(path, modelo) {
  b <- read_csv(path, col_types=cols(.default=col_character()), na=c("","NA"), trim_ws=FALSE, name_repair="minimal", show_col_types=FALSE, progress=FALSE)
  if(!identical(names(b),names(modelo))) stop("Estrutura CSV divergente: ",path)
  for(nm in names(modelo)) b[[nm]] <- converter(b[[nm]],modelo[[nm]],nm)
  as_tibble(b)
}
comparar <- function(rds,csv,chaves,fonte) {
  ord <- function(x) as_tibble(x)|>arrange(across(all_of(chaves)))|>select(all_of(names(x)))
  a<-ord(rds); b<-ord(csv); nomes<-identical(names(a),names(b)); tipos<-nomes&&identical(map_chr(a,tipo),map_chr(b,tipo)); dims<-identical(dim(a),dim(b)); cmp<-if(nomes&&tipos&&dims) all.equal(a,b,check.attributes=FALSE,tolerance=1e-12) else "estrutura divergente"; iguais<-isTRUE(cmp)
  list(dados=a, diagnostico=tibble(fonte=fonte,linhas_rds=nrow(a),linhas_csv=nrow(b),colunas_rds=ncol(a),colunas_csv=ncol(b),mesmos_nomes=nomes,mesmos_tipos=tipos,mesmas_dimensoes=dims,mesmos_valores=iguais,detalhe=if(iguais)"equivalentes" else paste(cmp,collapse=" | "),aprovado=nomes&&tipos&&dims&&iguais))
}
ler_par <- function(rds_nm,csv_nm,chaves,fonte) {
  r <- readRDS(entradas[[rds_nm]]); if(!is.data.frame(r)) stop("RDS inválido: ",fonte)
  comparar(r, ler_csv_modelo(entradas[[csv_nm]],r), chaves, fonte)
}
exigir <- function(d,cols,fonte) { a<-setdiff(cols,names(d)); if(length(a)) stop("Colunas ausentes em ",fonte,": ",paste(a,collapse=", ")) }
fmt_num <- function(x,d=0) if(length(x)==0||is.na(x[[1]])) "Não disponível" else formatC(as.numeric(x[[1]]),format="f",digits=d,big.mark=".",decimal.mark=",")
fmt_pct <- function(x,d=1) if(length(x)==0||is.na(x[[1]])) "Não disponível" else paste0(fmt_num(x,d),"%")
fmt_txt <- function(x,p="Não informado") if(length(x)==0||is.na(x[[1]])||!nzchar(str_trim(as.character(x[[1]])))) p else as.character(x[[1]])
escape_html <- function(x) { x<-ifelse(is.na(x),"",as.character(x)); x|>str_replace_all("&","&amp;")|>str_replace_all("<","&lt;")|>str_replace_all(">","&gt;")|>str_replace_all('"',"&quot;")|>str_replace_all("'","&#39;") }
slug <- function(x) { z<-suppressWarnings(iconv(x,from="UTF-8",to="ASCII//TRANSLIT")); z[is.na(z)]<-x[is.na(z)]; z<-z|>str_to_lower()|>str_replace_all("[^a-z0-9]+","-")|>str_replace_all("(^-+|-+$)","")|>str_sub(1,80); if_else(is.na(z)|z=="","carteira",z) }
nao_vazio <- function(x,min=100) file.exists(x)&&is.finite(file.info(x)$size)&&file.info(x)$size>=min
inventariar <- function(nome,path) tibble(arquivo=nome,caminho=norm(path,FALSE),existe=file.exists(path),tamanho_bytes=if(file.exists(path))file.info(path)$size else NA_real_,md5=md5(path))
copiar_validado <- function(a,b,overwrite=FALSE) { dir.create(dirname(b),recursive=TRUE,showWarnings=FALSE); ok<-file.copy(a,b,overwrite=overwrite,copy.mode=TRUE,copy.date=TRUE); if(!ok||!identical(md5(a),md5(b))) stop("Falha ao copiar: ",a) }
validacao <- function(teste,categoria,severidade,observado,criterio,resultado,obs="") tibble(teste,categoria,severidade,valor_observado=as.character(observado),criterio,resultado=isTRUE(resultado),nivel=if_else(isTRUE(resultado),"OK",str_to_upper(severidade)),observacao=obs)

# Ambiente e hashes
if(!file.exists(script_canonico)||!identical(norm(script_canonico,FALSE),norm(here("R","20_gerar_fichas_carteiras.R"),FALSE))) stop("Script fora do caminho canônico")
branch<-git_branch(); head<-git_head()
if(is.na(branch)||branch!=branch_esperada) stop("Branch divergente: ",branch)
if(is.na(head)||head!=commit_base) stop("HEAD divergente: ",head)
obrigatorios<-c(entradas,manifestos); faltantes<-obrigatorios[!file.exists(obrigatorios)]; if(length(faltantes)) stop("Arquivos ausentes:\n",paste(names(faltantes),faltantes,sep=": ",collapse="\n"))
ler_manifesto <- function(path,nome) { x<-read_csv(path,show_col_types=FALSE,na=c("","NA")); req<-c("caminho","tamanho_bytes","md5"); a<-setdiff(req,names(x)); if(length(a)) stop("Manifesto ",nome," sem: ",paste(a,collapse=", ")); x }
m18<-ler_manifesto(manifestos[["modulo_18"]],"18"); m19<-ler_manifesto(manifestos[["modulo_19"]],"19")
hash_man <- function(m,path,origem) { alvo<-basename(path); z<-m|>filter(basename(caminho)==alvo); if(nrow(z)!=1) stop("Arquivo não localizado unicamente no manifesto ",origem,": ",alvo); str_to_lower(z$md5[[1]]) }
hashes_esperados <- c(
  analise_csv=hash_man(m18,entradas[["analise_csv"]],"18"), analise_rds=hash_man(m18,entradas[["analise_rds"]],"18"),
  detalhe_csv=hash_man(m18,entradas[["detalhe_csv"]],"18"), detalhe_rds=hash_man(m18,entradas[["detalhe_rds"]],"18"),
  composicao_csv=hash_man(m18,entradas[["composicao_csv"]],"18"), composicao_rds=hash_man(m18,entradas[["composicao_rds"]],"18"),
  diagnostico_csv=hash_man(m18,entradas[["diagnostico_csv"]],"18"), diagnostico_rds=hash_man(m18,entradas[["diagnostico_rds"]],"18"),
  universo_csv=hash_man(m18,entradas[["universo_csv"]],"18"), universo_rds=hash_man(m18,entradas[["universo_rds"]],"18"),
  fichas_escolas_csv=hash_man(m19,entradas[["fichas_escolas_csv"]],"19"), fichas_escolas_rds=hash_man(m19,entradas[["fichas_escolas_rds"]],"19"),
  indice_escolas_csv=hash_man(m19,entradas[["indice_escolas_csv"]],"19"), indice_escolas_rds=hash_man(m19,entradas[["indice_escolas_rds"]],"19")
)
hashes_obs<-map_chr(entradas,md5); div<-names(hashes_obs)[str_to_lower(hashes_obs)!=str_to_lower(hashes_esperados)]; if(length(div)) stop("Hashes divergentes: ",paste(div,collapse=", "))

# Leitura
p1<-ler_par("analise_rds","analise_csv","assessora_gerencial_2026","analise_carteiras_assessoras")
p2<-ler_par("detalhe_rds","detalhe_csv",c("assessora_gerencial_2026","id_escola"),"carteira_escola_detalhe")
p3<-ler_par("composicao_rds","composicao_csv",c("assessora_gerencial_2026","metrica"),"composicao_operacional_carteiras")
p4<-ler_par("diagnostico_rds","diagnostico_csv","assessora_gerencial_2026","diagnostico_educacional_carteiras")
p5<-ler_par("universo_rds","universo_csv","id_escola","universo_institucional_carteiras")
p6<-ler_par("fichas_escolas_rds","fichas_escolas_csv","id_escola","base_fichas_escolas")
p7<-ler_par("indice_escolas_rds","indice_escolas_csv","id_escola","indice_fichas_escolas")
equiv_entradas<-bind_rows(p1$diagnostico,p2$diagnostico,p3$diagnostico,p4$diagnostico,p5$diagnostico,p6$diagnostico,p7$diagnostico); if(!all(equiv_entradas$aprovado)) stop("Divergência CSV-RDS")
analise<-p1$dados; detalhe<-p2$dados; composicao<-p3$dados; diagnostico<-p4$dados; universo<-p5$dados; fichas_escolas<-p6$dados; indice_escolas<-p7$dados

# Contratos
exigir(analise,c("assessora_gerencial_2026","numero_escolas","matriculas_anos_iniciais_total","turmas_anos_iniciais_total","soma_indice_operacional_carteira","media_indice_operacional_escolas","mediana_indice_operacional_escolas","minimo_indice_operacional_escolas","maximo_indice_operacional_escolas","soma_contribuicao_volume","soma_contribuicao_estrutural","soma_contribuicao_administrativa","media_score_dimensao_volume","media_score_dimensao_estrutural","media_score_dimensao_administrativa","participacao_maior_contribuicao_escola_pct","participacao_duas_maiores_contribuicoes_pct"),"analise")
exigir(detalhe,c("assessora_gerencial_2026","assessora_vinculo_administrativo","id_escola","codigo_inep","nome_canonico","matriculas_anos_iniciais","turmas_anos_iniciais","score_dimensao_volume","score_dimensao_estrutural","score_dimensao_administrativa","contribuicao_volume","contribuicao_estrutural","contribuicao_administrativa","indice_carga_potencial_operacional","resultados_educacionais_no_indice"),"detalhe")
exigir(composicao,c("assessora_gerencial_2026","metrica","valor","dimensao","tipo_metrica","uso_no_indice_operacional"),"composicao")
exigir(diagnostico,c("assessora_gerencial_2026","taxa_participacao_agregada_2026","proficiencia_2026_ponderada_avaliados","peso_maximo_no_indice_operacional","linhas_marcadas_uso_no_indice","advertencia"),"diagnostico")
exigir(indice_escolas,c("id_escola","arquivo_html","status_geracao"),"indice_escolas")

assessoras<-sort(unique(detalhe$assessora_gerencial_2026))
if(length(assessoras)!=11||nrow(analise)!=11||nrow(detalhe)!=53||nrow(composicao)!=99||nrow(diagnostico)!=11) stop("Universos divergentes")
if(anyDuplicated(detalhe$id_escola)>0||any(detalhe$id_escola%in%ids_excluidos)||any(is.na(detalhe$assessora_gerencial_2026))) stop("Problema no universo operacional")
if(any(coalesce(logico(detalhe$resultados_educacionais_no_indice,"resultados_educacionais_no_indice"),FALSE))) stop("Resultados educacionais no índice")
if(any(diagnostico$peso_maximo_no_indice_operacional!=0)||any(diagnostico$linhas_marcadas_uso_no_indice!=0)) stop("Diagnóstico educacional com uso no índice")
if(!setequal(assessoras,unique(analise$assessora_gerencial_2026))||!setequal(assessoras,unique(composicao$assessora_gerencial_2026))||!setequal(assessoras,unique(diagnostico$assessora_gerencial_2026))) stop("Conjuntos de assessoras divergentes")

# Links e base
links<-indice_escolas|>select(id_escola,arquivo_html_escola=arquivo_html,status_ficha_escola=status_geracao)
detalhe<-detalhe|>left_join(links,by="id_escola")
referencia<-analise|>summarise(ref_carga=mean(soma_indice_operacional_carteira),ref_indice=mean(media_indice_operacional_escolas),ref_volume=mean(media_score_dimensao_volume),ref_estrutura=mean(media_score_dimensao_estrutural),ref_administracao=mean(media_score_dimensao_administrativa))
base<-analise|>crossing(referencia)|>mutate(diferenca_carga_referencia=soma_indice_operacional_carteira-ref_carga,diferenca_indice_referencia=media_indice_operacional_escolas-ref_indice,slug=slug(assessora_gerencial_2026),html_nome=paste0("carteira_",slug,".html"),g_dim=paste0("carteira_",slug,"_dimensoes.png"),g_comp=paste0("carteira_",slug,"_composicao.png"),g_part=paste0("carteira_",slug,"_participacao.png"),g_prof=paste0("carteira_",slug,"_proficiencia.png"))|>arrange(assessora_gerencial_2026)

tema<-function() theme_minimal(base_size=11)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank(),legend.position="bottom")
graf_dim<-function(x,path){d<-tibble(dimensao=factor(c("Volume","Estrutura","Complexidade administrativa"),levels=c("Complexidade administrativa","Estrutura","Volume")),Carteira=c(x$media_score_dimensao_volume,x$media_score_dimensao_estrutural,x$media_score_dimensao_administrativa),Referencia=c(x$ref_volume,x$ref_estrutura,x$ref_administracao))|>pivot_longer(c(Carteira,Referencia),names_to="grupo",values_to="valor");p<-ggplot(d,aes(valor,dimensao,fill=grupo))+geom_col(position="dodge")+labs(title="Dimensões operacionais",subtitle="Referência agregada não ordinal; educação fora do índice.",x="Escore médio",y=NULL,fill=NULL)+tema();ggsave(path,p,width=8,height=4.5,dpi=150)}
graf_comp<-function(d,path){p<-ggplot(d,aes(valor,fct_reorder(metrica,valor),fill=dimensao))+geom_col()+labs(title="Composição operacional",x="Valor",y=NULL,fill="Dimensão")+tema();ggsave(path,p,width=8,height=5.5,dpi=150)}
graf_edu<-function(d,path,tipo){if(tipo=="participacao"){valor<-d$taxa_participacao_agregada_2026;ttl<-"Participação agregada em 2026";yl<-"Participação (%)"}else{valor<-d$proficiencia_2026_ponderada_avaliados;ttl<-"Proficiência observada em 2026";yl<-"Proficiência"};p<-ggplot(tibble(indicador="2026",valor=valor),aes(indicador,valor))+geom_col()+geom_text(aes(label=round(valor,1)),vjust=-.3)+labs(title=ttl,subtitle="Diagnóstico descritivo e não causal.",x=NULL,y=yl)+tema();ggsave(path,p,width=8,height=4.5,dpi=150)}

css<-paste0("<style>body{font-family:Arial,sans-serif;background:#f4f6f8;color:#263238;margin:0}.pagina{max-width:1100px;margin:auto;background:white;padding:28px 36px}h1{font-size:25px}h2{font-size:18px;border-bottom:2px solid #cfd8dc;padding-bottom:6px;margin-top:26px}.grid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px}.card{border:1px solid #cfd8dc;border-radius:6px;padding:10px;background:#fafafa}.rot{font-size:11px;text-transform:uppercase;color:#607d8b;font-weight:bold}.val{font-size:16px;margin-top:4px}.nota{border-left:4px solid #78909c;background:#f5f7f8;padding:12px;margin:14px 0;font-size:12px}table{width:100%;border-collapse:collapse;font-size:12px}th,td{border:1px solid #cfd8dc;padding:6px}th{background:#eceff1}img{width:100%;max-width:900px;border:1px solid #cfd8dc;margin:12px 0}.rodape{margin-top:28px;border-top:1px solid #cfd8dc;padding-top:10px;font-size:10px;color:#607d8b}@media print{body{background:white}.pagina{max-width:none}.quebra{page-break-before:always}}</style>")
card<-function(r,v)paste0("<div class='card'><div class='rot'>",escape_html(r),"</div><div class='val'>",escape_html(v),"</div></div>")
tabela_escolas<-function(d){lin<-map_chr(seq_len(nrow(d)),function(i){x<-d[i,];link<-if(!is.na(x$arquivo_html_escola)&&x$status_ficha_escola=="SUCESSO")paste0("<a href='../../fichas_escolas/execucao_20260729_233939/html/",escape_html(basename(x$arquivo_html_escola)),"'>abrir ficha</a>")else"não disponível";paste0("<tr><td>",escape_html(x$id_escola),"</td><td>",escape_html(x$nome_canonico),"</td><td>",escape_html(x$codigo_inep),"</td><td>",escape_html(fmt_txt(x$assessora_vinculo_administrativo)),"</td><td>",fmt_num(x$indice_carga_potencial_operacional,1),"</td><td>",fmt_num(x$score_dimensao_volume,1),"</td><td>",fmt_num(x$score_dimensao_estrutural,1),"</td><td>",fmt_num(x$score_dimensao_administrativa,1),"</td><td>",link,"</td></tr>")});paste0("<table><thead><tr><th>ID</th><th>Escola</th><th>INEP</th><th>Vínculo administrativo</th><th>Índice</th><th>Volume</th><th>Estrutura</th><th>Administração</th><th>Ficha</th></tr></thead><tbody>",paste(lin,collapse=""),"</tbody></table>")}
render_html<-function(x,d){paste0("<!DOCTYPE html><html lang='pt-BR'><head><meta charset='UTF-8'><title>Carteira ",escape_html(x$assessora_gerencial_2026),"</title>",css,"</head><body><div class='pagina'><h1>Carteira de ",escape_html(str_to_title(x$assessora_gerencial_2026)),"</h1><h2>Volume e carga operacional</h2><div class='grid'>",card("Escolas",fmt_num(x$numero_escolas)),card("Matrículas",fmt_num(x$matriculas_anos_iniciais_total)),card("Turmas",fmt_num(x$turmas_anos_iniciais_total)),card("Carga operacional total",fmt_num(x$soma_indice_operacional_carteira,1)),card("Índice médio",fmt_num(x$media_indice_operacional_escolas,1)),card("Índice mediano",fmt_num(x$mediana_indice_operacional_escolas,1)),card("Índice mínimo",fmt_num(x$minimo_indice_operacional_escolas,1)),card("Índice máximo",fmt_num(x$maximo_indice_operacional_escolas,1)),card("Diferença para referência",fmt_num(x$diferenca_carga_referencia,1)),"</div><div class='nota'>A referência é agregada e não ordinal. O índice não equivale à carga real total e não permite, isoladamente, afirmar sobrecarga ou subutilização.</div><img src='../graficos/",escape_html(x$g_dim),"'><h2>Composição operacional</h2><img src='../graficos/",escape_html(x$g_comp),"'><h2>Diagnóstico educacional contextual</h2><img src='../graficos/",escape_html(x$g_part),"'><img src='../graficos/",escape_html(x$g_prof),"'><div class='nota'>Os resultados educacionais são descritivos, observacionais e sujeitos a diferenças de participação e composição. Não integram o índice operacional, não representam efeito do assessoramento e não avaliam o desempenho da assessora.</div><h2>Escolas da carteira</h2>",tabela_escolas(d),"<div class='rodape'>Execução ",id_execucao," — módulo 20.</div></div></body></html>")}

resultado<-vector("list",nrow(base))
for(i in seq_len(nrow(base))){x<-base[i,];a<-x$assessora_gerencial_2026[[1]];d<-detalhe|>filter(assessora_gerencial_2026==a)|>arrange(nome_canonico);c<-composicao|>filter(assessora_gerencial_2026==a);g<-diagnostico|>filter(assessora_gerencial_2026==a);hp<-file.path(pasta_html,x$html_nome);dp<-file.path(pasta_graficos,x$g_dim);cp<-file.path(pasta_graficos,x$g_comp);pp<-file.path(pasta_graficos,x$g_part);fp<-file.path(pasta_graficos,x$g_prof);status<-"SUCESSO";erro<-NA_character_;tryCatch({graf_dim(x,dp);graf_comp(c,cp);graf_edu(g,pp,"participacao");graf_edu(g,fp,"proficiencia");writeLines(render_html(x,d),hp,useBytes=TRUE)},error=function(e){status<<-"ERRO";erro<<-conditionMessage(e)});resultado[[i]]<-tibble(assessora_gerencial_2026=a,status_geracao=status,mensagem_erro=erro,arquivo_html=hp,html_existe=nao_vazio(hp,1000),arquivo_grafico_dimensoes=dp,grafico_dimensoes_existe=nao_vazio(dp,1000),arquivo_grafico_composicao=cp,grafico_composicao_existe=nao_vazio(cp,1000),arquivo_grafico_participacao=pp,grafico_participacao_existe=nao_vazio(pp,1000),arquivo_grafico_proficiencia=fp,grafico_proficiencia_existe=nao_vazio(fp,1000))}
resultado<-bind_rows(resultado);if(any(resultado$status_geracao!="SUCESSO")){write_csv(resultado,file.path(pasta_execucao,"06_resultado_geracao_fichas.csv"),na="");stop("Falha na geração")}

secoes<-map_chr(seq_len(nrow(base)),function(i){x<-base[i,];d<-detalhe|>filter(assessora_gerencial_2026==x$assessora_gerencial_2026[[1]])|>arrange(nome_canonico);h<-render_html(x,d);corpo<-str_match(h,"(?s)<body><div class='pagina'>(.*)</div></body>")[,2];paste0("<section class='pagina quebra'>",corpo,"</section>")})
html_comp<-file.path(pasta_candidatos,"fichas_carteiras_compiladas.html");writeLines(paste0("<!DOCTYPE html><html lang='pt-BR'><head><meta charset='UTF-8'>",css,"</head><body>",paste(secoes,collapse="\n"),"</body></html>"),html_comp,useBytes=TRUE)
pdf_comp<-file.path(pasta_candidatos,"fichas_carteiras_compiladas.pdf");status_pdf<-"NAO_GERADO";mensagem_pdf<-NA_character_;if(TENTAR_GERAR_PDF_COMPILADO&&requireNamespace("pagedown",quietly=TRUE)){tryCatch({pagedown::chrome_print(input=html_comp,output=pdf_comp,wait=2);if(nao_vazio(pdf_comp,10000))status_pdf<-"GERADO"},error=function(e)mensagem_pdf<<-conditionMessage(e))}else mensagem_pdf<-"pagedown indisponível"

base_saida<-base|>transmute(assessora_gerencial_2026,numero_escolas,matriculas_anos_iniciais_total,turmas_anos_iniciais_total,carga_operacional_total=soma_indice_operacional_carteira,indice_operacional_medio=media_indice_operacional_escolas,indice_operacional_mediano=mediana_indice_operacional_escolas,indice_operacional_minimo=minimo_indice_operacional_escolas,indice_operacional_maximo=maximo_indice_operacional_escolas,dimensao_volume_media=media_score_dimensao_volume,dimensao_estrutural_media=media_score_dimensao_estrutural,dimensao_administrativa_media=media_score_dimensao_administrativa,contribuicao_volume_total=soma_contribuicao_volume,contribuicao_estrutura_total=soma_contribuicao_estrutural,contribuicao_administracao_total=soma_contribuicao_administrativa,participacao_maior_escola_pct=participacao_maior_contribuicao_escola_pct,participacao_duas_maiores_pct=participacao_duas_maiores_contribuicoes_pct,ref_carga,ref_indice,ref_volume,ref_estrutura,ref_administracao,diferenca_carga_referencia,diferenca_indice_referencia,html_nome,g_dim,g_comp,g_part,g_prof)
indice_saida<-resultado|>transmute(assessora_gerencial_2026,status_geracao,mensagem_erro,arquivo_html=file.path("resultados","fichas_carteiras",paste0("execucao_",id_execucao),"html",basename(arquivo_html)),arquivo_grafico_dimensoes=file.path("resultados","fichas_carteiras",paste0("execucao_",id_execucao),"graficos",basename(arquivo_grafico_dimensoes)),arquivo_grafico_composicao=file.path("resultados","fichas_carteiras",paste0("execucao_",id_execucao),"graficos",basename(arquivo_grafico_composicao)),arquivo_grafico_participacao=file.path("resultados","fichas_carteiras",paste0("execucao_",id_execucao),"graficos",basename(arquivo_grafico_participacao)),arquivo_grafico_proficiencia=file.path("resultados","fichas_carteiras",paste0("execucao_",id_execucao),"graficos",basename(arquivo_grafico_proficiencia)),html_existe,grafico_dimensoes_existe,grafico_composicao_existe,grafico_participacao_existe,grafico_proficiencia_existe)
dicionario<-imap_dfr(list(base_fichas_carteiras=base_saida,indice_fichas_carteiras=indice_saida),function(dados,produto)tibble(produto,ordem_coluna=seq_along(dados),variavel=names(dados),classe_r=map_chr(dados,~paste(class(.x),collapse=" | ")),observacao_metodologica=case_when(str_detect(names(dados),"participacao|proficiencia")~"Resultado educacional descritivo; não avalia a assessora.",str_detect(names(dados),"indice|carga|dimensao|contribuicao")~"Medida operacional relativa; não equivale à carga real total.",TRUE~"Usar com leitura contextual e qualitativa.")))
cands<-file.path(pasta_candidatos,basename(saidas));names(cands)<-names(saidas);write_csv(base_saida,cands[["base_csv"]],na="");saveRDS(base_saida,cands[["base_rds"]]);write_csv(indice_saida,cands[["indice_csv"]],na="");saveRDS(indice_saida,cands[["indice_rds"]]);write_csv(dicionario,cands[["dicionario_csv"]],na="")
eq_base<-comparar(readRDS(cands[["base_rds"]]),ler_csv_modelo(cands[["base_csv"]],readRDS(cands[["base_rds"]])),"assessora_gerencial_2026","base_fichas_carteiras")
eq_ind<-comparar(readRDS(cands[["indice_rds"]]),ler_csv_modelo(cands[["indice_csv"]],readRDS(cands[["indice_rds"]])),"assessora_gerencial_2026","indice_fichas_carteiras");equiv_candidatos<-bind_rows(eq_base$diagnostico,eq_ind$diagnostico)

arquivos_cand<-list.files(pasta_candidatos,recursive=TRUE,full.names=TRUE);arquivos_cand<-arquivos_cand[!file.info(arquivos_cand)$isdir];manifesto_cand<-map_dfr(arquivos_cand,~inventariar(str_remove(norm(.x,FALSE),paste0("^",fixed(norm(pasta_candidatos,FALSE)),"/?")),.x))
n_html<-sum(str_detect(manifesto_cand$arquivo,"^html/.+\\.html$"));n_graf<-sum(str_detect(manifesto_cand$arquivo,"^graficos/.+\\.png$"))
validacoes<-bind_rows(validacao("Branch","execucao","erro",branch,branch_esperada,branch==branch_esperada),validacao("HEAD","execucao","erro",head,commit_base,head==commit_base),validacao("Entradas equivalentes","integridade","erro",sum(equiv_entradas$aprovado),"7",all(equiv_entradas$aprovado)),validacao("Carteiras","universo","erro",nrow(analise),"11",nrow(analise)==11),validacao("Escolas","universo","erro",nrow(detalhe),"53",nrow(detalhe)==53),validacao("Composição","universo","erro",nrow(composicao),"99",nrow(composicao)==99),validacao("Diagnóstico","universo","erro",nrow(diagnostico),"11",nrow(diagnostico)==11),validacao("Base","produto","erro",nrow(base_saida),"11",nrow(base_saida)==11),validacao("Índice fichas","produto","erro",nrow(indice_saida),"11",nrow(indice_saida)==11),validacao("HTMLs","produto","erro",n_html,"11",n_html==11),validacao("Gráficos","produto","erro",n_graf,"44",n_graf==44),validacao("HTML compilado","produto","erro",nao_vazio(html_comp,10000),"TRUE",nao_vazio(html_comp,10000)),validacao("Candidatos equivalentes","integridade","erro",sum(equiv_candidatos$aprovado),"2",all(equiv_candidatos$aprovado)))
erros<-validacoes|>filter(!resultado&severidade=="erro")

manifesto_entradas<-imap_dfr(entradas,~inventariar(.y,.x))|>mutate(md5_esperado=hashes_esperados[arquivo],hash_aprovado=str_to_lower(md5)==str_to_lower(md5_esperado))
write_csv(tribble(~parametro,~valor,"universo_operacional","53","carteiras","11","peso_volume","0,40","peso_estrutura","0,35","peso_administracao","0,25","peso_educacional","0","ranking","não produzido","faixa","não produzida","cenario","não produzido","pdf",status_pdf),file.path(pasta_execucao,"01_parametros_execucao.csv"),na="")
write_csv(manifesto_entradas,file.path(pasta_execucao,"02_manifesto_arquivos_entrada.csv"),na="");write_csv(equiv_entradas,file.path(pasta_execucao,"03_equivalencia_csv_rds_entradas.csv"),na="");write_csv(base_saida,file.path(pasta_execucao,"04_base_fichas_candidata.csv"),na="");write_csv(indice_saida,file.path(pasta_execucao,"05_indice_fichas_candidato.csv"),na="");write_csv(resultado,file.path(pasta_execucao,"06_resultado_geracao_fichas.csv"),na="");write_csv(manifesto_cand,file.path(pasta_execucao,"07_manifesto_resultados_candidatos.csv"),na="");write_csv(validacoes,file.path(pasta_execucao,"08_validacao_final.csv"),na="");write_csv(equiv_candidatos,file.path(pasta_execucao,"09_equivalencia_csv_rds_candidatos.csv"),na="");write_csv(dicionario,file.path(pasta_execucao,"10_dicionario_candidato.csv"),na="");write_lines(capture.output(sessionInfo()),file.path(pasta_execucao,"11_session_info.txt"));write_csv(tibble(campo=c("id_execucao","instante","branch","commit_base","caminho_script","md5_script","status_pdf","mensagem_pdf"),valor=c(id_execucao,format(Sys.time(),"%Y-%m-%d %H:%M:%S %z"),branch,head,norm(script_canonico),md5(script_canonico),status_pdf,coalesce(mensagem_pdf,""))),file.path(pasta_execucao,"12_identificacao_execucao.csv"),na="");write_lines(c(paste0("Execução: ",id_execucao),paste0("Branch: ",branch),paste0("Commit-base: ",head),paste0("MD5: ",md5(script_canonico)),"Carteiras: 11","Escolas: 53","HTMLs: 11","Gráficos: 44",paste0("PDF: ",status_pdf),paste0("Erros críticos: ",nrow(erros)),"","Resultados educacionais fora do índice.","Sem ranking, faixa, percentil ou cenário."),file.path(pasta_execucao,"13_resumo_execucao.txt"))
if(nrow(erros)>0) stop("Erros críticos; consulte 08_validacao_final.csv")

anteriores<-saidas[file.exists(saidas)];if(length(anteriores))walk2(anteriores,names(anteriores),~copiar_validado(.x,file.path(pasta_historico,basename(.x)),FALSE));manifesto_hist<-if(length(anteriores))imap_dfr(anteriores,~inventariar(.y,file.path(pasta_historico,basename(.x))))else tibble(arquivo=character(),caminho=character(),existe=logical(),tamanho_bytes=double(),md5=character());write_csv(manifesto_hist,file.path(pasta_execucao,"14_manifesto_preservacao_historica.csv"),na="")
promovidos<-character();tryCatch({for(nm in names(saidas)){dest<-saidas[[nm]];cand<-cands[[nm]];if(file.exists(dest))copiar_validado(dest,file.path(pasta_rollback,basename(dest)),FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);ok<-file.copy(cand,dest,overwrite=TRUE);if(!ok||!identical(md5(cand),md5(dest)))stop("Falha na promoção: ",nm);promovidos<-c(promovidos,nm)}},error=function(e){for(nm in rev(promovidos)){dest<-saidas[[nm]];rb<-file.path(pasta_rollback,basename(dest));if(file.exists(rb))file.copy(rb,dest,overwrite=TRUE)else if(file.exists(dest))file.remove(dest)};stop("Promoção falhou; rollback executado: ",conditionMessage(e))})
if(dir.exists(pasta_final))stop("Pasta final já existe: ",pasta_final);dir.create(pasta_final,recursive=TRUE,showWarnings=FALSE);itens<-list.files(pasta_candidatos,all.files=TRUE,full.names=TRUE,no..=TRUE);ok<-file.copy(itens,pasta_final,recursive=TRUE,overwrite=FALSE,copy.mode=TRUE,copy.date=TRUE);if(length(ok)!=length(itens)||!all(ok)){unlink(pasta_final,recursive=TRUE,force=TRUE);stop("Falha na promoção dos resultados")}
manifesto_produtos<-imap_dfr(saidas,~inventariar(.y,.x));arq_prom<-list.files(pasta_final,recursive=TRUE,full.names=TRUE);arq_prom<-arq_prom[!file.info(arq_prom)$isdir];manifesto_prom<-map_dfr(arq_prom,~inventariar(str_remove(norm(.x,FALSE),paste0("^",fixed(norm(pasta_final,FALSE)),"/?")),.x));ver_res<-manifesto_cand|>select(arquivo,md5_candidato=md5)|>left_join(manifesto_prom|>select(arquivo,md5_promovido=md5),by="arquivo")|>mutate(hash_igual=str_to_lower(md5_candidato)==str_to_lower(md5_promovido));ver_prod<-imap_dfr(cands,~inventariar(.y,.x))|>select(arquivo,md5_candidato=md5)|>left_join(manifesto_produtos|>select(arquivo,md5_promovido=md5),by="arquivo")|>mutate(hash_igual=str_to_lower(md5_candidato)==str_to_lower(md5_promovido));write_csv(manifesto_produtos,file.path(pasta_execucao,"15_manifesto_produtos_modulo_20.csv"),na="");write_csv(manifesto_prom,file.path(pasta_execucao,"16_manifesto_resultados_promovidos.csv"),na="");write_csv(ver_res,file.path(pasta_execucao,"17_verificacao_promocao_resultados.csv"),na="");write_csv(ver_prod,file.path(pasta_execucao,"18_verificacao_promocao_produtos.csv"),na="");if(any(!ver_res$hash_igual)||any(!ver_prod$hash_igual))stop("Falha na verificação final")
message("Módulo 20 concluído com sucesso.\nExecução: ",id_execucao,"\nCarteiras: 11\nEscolas: 53\nHTMLs: 11\nGráficos: 44\nPDF: ",status_pdf,"\nResultados: ",pasta_final,"\nDocumentação: ",pasta_execucao,"\nMD5: ",md5(script_canonico))
