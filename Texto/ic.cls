%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Classe de documento LATEX para confecção de trabalhos de conclusão de      %%
%% curso e dissertações de mestrado do Departamento de Tecnologia da          %%
%% Informação da Universidade Federal de Alagoas                              %%
%%                                                                            %%
%% Autores:                                                                   %%
%%       Rodrigo Paes - rbp@les.inf.puc-rio.br                                %%
%%       Hyggo Almeida - hyggo@dee.ufcg.edu.br                                %%
%%                                                                            %%
%% Data: 19 de fevereiro de 2004                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Modificações:                                                              %%
%% Fontes (lettrine), layout inicial dos capítulos (adaptado de B. Lopes),    %%
%% hyperlinks, annotations com abstract das referências no pdf (article).     %%
%%                                                                            %%
%% Adaptado por:                                                              %%
%%       Rian Pinheiro - rian.gabriel@gmail.com                               %%
%%       Ivan Martins - ivn.martins@gmail.com                                 %%
%%                                                                            %%
%% Data: Outubro de 2010                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Modificações:                                                              %%
%% Hifenização automática de referências ao fim da linha, adição de símbolos  %%
%% (bbding), múltiplas linhas por campo da tabela (multirow)                  %% 
%%                                                                            %%
%% Adaptado por:                                                              %%
%%       Paulo Rogério - progerio07@gmail.com                                 %%
%%                                                                            %%
%% Data: Fevereiro de 2012                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




\NeedsTeXFormat{LaTeX2e}
   \ProvidesClass{ic}[2004/02/18 v1.0 Modelo IC-UFAL de Trabalhos de Conclusao 
de Curso e Dissertacoes]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definição de extensão da classe REPORT - Papel A4, FONTE 12pt              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\LoadClass[a4paper,12pt]{report} %210 mm X 294 mm
%%------------------------------------------------------------------------------
%% Habilita a utilização de arroba
%%------------------------------------------------------------------------------
\makeatletter

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      LISTA DE PACOTES REQUERIDOS PARA A UTILIZAÇÃO DESTA CLASSE            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[T1]{fontenc} % Definição da codificação das fontes como T1
\RequirePackage{graphicx} % Inclusão de imagens
\RequirePackage{setspace} % Definição dos espaçamemtos entre linhas
\RequirePackage{calc} % Aceitação de notação infixa
\RequirePackage{fancyhdr} % Personalização de cabeçalhos e rodapés
\RequirePackage{listings} % Inclusão de código fonte de diversas linguagens
\RequirePackage{pslatex} % Utilização de fontes PostScript Tipo 1
\RequirePackage[utf8]{inputenc} % Uso caracteres especiais UTF8
\RequirePackage[a4paper]{geometry} % Para ajustar o papel
\RequirePackage[portuguese,english]{babel} % Para habilitar os macros de línguas
\RequirePackage{natbib} % Estilo das referências
\RequirePackage{url} % Para lidar com URLs
\RequirePackage{doc} % Contém a definição da logomarca do BibTex
\RequirePackage{color,colortbl} % Permite "pintar" as tabelas
\RequirePackage{ifthen} % Utilização de condicionais em latex

%%%%%%%%%%%%%%%%%%%%%%%%%% [Rian,Ivan] Início Modificações %%%%%%%%%%%%%%%%%%%%%
\RequirePackage{amsmath} %
%\RequirePackage{amsfonts}
%   \newcommand\hmmax{0} % default 3

\RequirePackage{bbm} 
%\RequirePackage{amssymb} %
%\RequirePackage{amscd} %
\RequirePackage{units} %
\RequirePackage{undertilde} % Utilização de caracteres com til em baixo
\RequirePackage{rotating}

\RequirePackage[ruled,chapter]{algorithm}
\RequirePackage{algpseudocode}
\floatname{algorithm}{Algoritmo}
% \renewcommand{\algorithmname}{Lista de Algoritmos}
\newenvironment{myequation}[2]
{\begin{equation}\label{#1}\addcontentsline{loe}{table}{\theequation~ #2}}
{\end{equation}}

\newcommand{\mequation}[3]{
\begin{equation}\label{#1}\addcontentsline{loe}{table}{\theequation~ #2}
#3
\end{equation}}

%%------------------------------------------------------------------------------
%% Fontes
%%------------------------------------------------------------------------------
\RequirePackage{fourier}  
\newcommand\NomeFonte{Fourier-GUT\textit{enberg}}
% \RequirePackage[euler-digits]{eulervm} 
\newcommand\NomeFonteMat{Euler Virtual Math}
\RequirePackage{eucal} 
\RequirePackage{lettrine} %
\newcommand{\maior}{\text{~\large \textgreater~}}
\newcommand{\menor}{\text{~\large \textless~}}

% \RequirePackage{yfonts} %

\RequirePackage{tocloft} % Pacote para reconfigurar recuo do sumário
\RequirePackage{tikz} % Pacote para desenhos
\RequirePackage{xcolor} % Definição das cores
\RequirePackage{bm} % Pacote para criação do modelo de RSSF (fig:model_rssf)
\RequirePackage{float} %
\RequirePackage{pdfcomment} % Comentário do Abstract
\RequirePackage{fancybox}
\RequirePackage{multicol}
% \RequirePackage[dvips]{hyperref} %
\RequirePackage[]{hyperref} %
\RequirePackage{subfigurequickfix} % Workaround By Marcelo Queiroz to fix the issue about that package ...
\RequirePackage{booktabs} %
\RequirePackage{wallpaper} % Pacote para incluir imagens em background

\definecolor{light_gray}{gray}{0.85}
\definecolor{Red}{rgb}{0.5,0,0}
\definecolor{Blue}{rgb}{0,0,0.5}

\newcommand\NomeFonteCap{Art Nouveau Caps}
\input{ArtNouvc.fd}
% \newcommand*\myfont{\fontsize{60}{75}\usefont{U}{ArtNouvc}{xl}{n}}
\newcommand*\myfont{\fontsize{60}{75}\bfseries}
\setcounter{DefaultLines}{3}

%%------------------------------------------------------------------------------
%% Redefinindo capítulo
%%------------------------------------------------------------------------------
\newcounter{mychaptercounter}
\newenvironment{mychapterenviroment}{\refstepcounter{mychaptercounter}}{}
\newcommand{\mychapter}[2]{
\stepcounter{chapter}
\chapter*{
\begin{mychapterenviroment}
\label{#2}
\end{mychapterenviroment}
\begin{flushright}
\normalfont\myfont\arabic{mychaptercounter}\normalfont\Huge\bf\\
\vspace{1em}
#1
\end{flushright}
}
\addcontentsline{toc}{chapter}{\arabic{mychaptercounter}\hspace{1em}#1}
}

%%------------------------------------------------------------------------------
%% Comando para colocar uma inicialização em cada capítulo
%%------------------------------------------------------------------------------
% \newcommand{\initialcitation}[4]{
% \begin{raggedleft}
% {\it``#1''\\\vspace{1em}{\em{\bf#2}\\#3 (#4)}\\}
% \end{raggedleft}
% \vspace{2em}
% }

%%------------------------------------------------------------------------------
%% Para a fonte da lettrine ser a que defini
%%------------------------------------------------------------------------------
\renewcommand{\LettrineFontHook}{}

%%------------------------------------------------------------------------------
%% Opções do pacote lettrine
%%------------------------------------------------------------------------------
\setcounter{DefaultLines}{2}
\renewcommand{\DefaultLoversize}{0.1}
\renewcommand{\DefaultLraise}{0}
\setlength{\DefaultNindent}{0.1em}
\setlength{\DefaultSlope}{-0.5em}


% Comentário do abstract
\newcommand{\comAbstract}[3]{
\pdfmarkupcomment[color=white,author=#2,markup=Squiggly]{#1}{#3}
}
\newcommand{\doi}[1]{\textbf{DOI}~{\href{http://dx.doi.org/#1}{\color{Blue}#1}}}


% Resumo do capítulo
\newcommand{\resumocap}[1]{
\vfill
\begin{center}
\fbox{
\colorbox{light_gray}{
\parbox[t]{.8\linewidth}{
#1
}}}
\end{center}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%% [Rian,Ivan] Fim Modificações %%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%% [Paulo] Início Modificações %%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage{cite} % Hifenização automática de referências ao fim das linhas
\RequirePackage{bbding} % Símbolos diferentes para listas de itens e outros
\RequirePackage{multirow} % Múltiplas linhas por campo da tabela

%%%%%%%%%%%%%%%%%%%%%%%%%%% [Paulo] Fim Modificações %%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                DECLARAÇÃO DE OPÇÕES: DISSERTAÇÃO e TCC                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% Define TCC como padrão
%%------------------------------------------------------------------------------
\newboolean{tcc}
\setboolean{tcc}{false}

%%------------------------------------------------------------------------------
%% Declara a opção de dissertação
%%------------------------------------------------------------------------------
\DeclareOption{dissertacao}{%
    \setboolean{tcc}{true}
    \typeout{Tipo de Documento: Dissertacao de Mestrado.}
}

%%------------------------------------------------------------------------------
%% Declara a opção de TCC
%%------------------------------------------------------------------------------
\DeclareOption{tcc}{%
    \setboolean{tcc}{false}
    \typeout{Tipo de Documento: Trabalho de Conclusao de Curso.}
}

%%------------------------------------------------------------------------------
%% Executa as opções
%%------------------------------------------------------------------------------
\ProcessOptions\relax


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 DEFINIÇÃO DO LAYOUT DO DOCUMENTO                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%%  TAMANHOS VERTICAIS - COPIADO DO ESTILO PADRÃO DA ABNT - Pacote abntex.cls
%%------------------------------------------------------------------------------
% A distancia entre o topo do cabeçalho e o topo do texto é de 1cm, 
% isto é, 1cm=\headheight+\headsep
% a profundidade do cabeçalho, adicionando 2mm.
\setlength{\headsep}{1.2cm-\headheight}
% A distância entre a borda do papel e o número deve ser de 2cm
% 2cm=\topmargin+\voffset+1in
\setlength{\topmargin}{2cm-1in-\voffset}
% A borda inferior deve ser de 2cm
% \paperheight=\topmargin+\voffset+1in+\headheight+\headsep+\textheight+2cm
\setlength{\textheight}%
{\paperheight-\topmargin-\voffset-1in-\headheight-\headsep-2cm}

%%------------------------------------------------------------------------------
%% TAMANHOS HORIZONTAIS - COPIADO DO ESTILO PADRÃO DA ABNT - Pacote abntex.cls
%%------------------------------------------------------------------------------
% A margem esquerda é de 3cm e a direita é igual a 2cm.
\setlength{\oddsidemargin}{3cm-\hoffset-1in}
% para compatibilidade com impressão frente e verso, o tamanho das margens 
%deve ser alterado
\setlength{\evensidemargin}{2cm-\hoffset-1in}
% \paperwidth=\textwidth+\oddsidemargin+\hoffset+1in+2cm
\setlength{\textwidth}{\paperwidth-\oddsidemargin-\hoffset-1in-2cm}

\newenvironment{changemargin}[2]{%
\begin{list}{}{%
\setlength{\topsep}{0pt}%
\setlength{\leftmargin}{#1}%
\setlength{\rightmargin}{#2}%
\setlength{\listparindent}{\parindent}%
\setlength{\itemindent}{\parindent}%
\setlength{\parsep}{\parskip}%
}%
\item[]}{\end{list}}

%%------------------------------------------------------------------------------
%% Define o cabeçalho das páginas
%%------------------------------------------------------------------------------
% Limpa qualquer configuração anterior
\fancyhf{}

% Define a aparição do nome da seção no cabeçalho(esquerda)
\lhead{\rightmark} 

% Define a aparição do número da página no cabeçalho(direita)
\rhead{\thepage} % Número da página

% Define a largura da barra
\renewcommand{\headrulewidth}{0.6pt} 

% Define a altura do cabeçalho
\addtolength{\headheight}{3.05pt} 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           DEFINIÇÃO DOS COMANDOS                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% Definição do comando CAPA
%%----------------------------------------------------------------------------
\newcommand{\capa}{%
    \thispagestyle{empty}%
    \begin{flushright} % Alinhamento à direita
      \href{http://www.ic.ufal.br}{\includegraphics[width=.3\linewidth]{IC.pdf}}\\%
% [Rian] Adicionado href
      \Large \vspace*{2cm} \ifthenelse{\boolean{tcc}}{Trabalho de Conclusão de 
Curso}{Dissertação de Mestrado}\\%
      \vspace*{2cm} \LARGE \textbf{\@titulo} \\%
      \vspace*{2cm}%
      \Large \href{\@autorPagina}{\color{black}{\@autorNome}} \\ \normalsize %
\texttt{\@autorEmail} \\ % [Rian] Adicionado href
      \ifthenelse{\boolean{autorDoisExiste}}{\Large \@autorDoisNome \\ %
\normalsize \texttt{\@autorDoisEmail}\\}{}%
      \vspace*{2cm}%
      \Large%
      \ifthenelse{\boolean{orientadorDoisExiste}}{Orientadores:}{Orientador:}%
      \\
      \vspace*{0.2cm}%
      \large
      \href{\@orientadorPagina}{\color{black}\@orientadorNome}\\%
% [Rian] Adicionado href
      %\@orientadorDoisNome\\
      \href{\@orientadorDoisPagina}{\color{black}\@orientadorDoisNome}\\%
      \@orientadorTresNome\\
      \normalsize \vfill Maceió, \@data%
    \end{flushright}%
    \folhaDeRosto%
    \catalogacao% 
%    \aprovacao%
    \renewcommand{\thepage}{\roman{page}} \setcounter{page}{0}%
    \normalsize%
}


%%------------------------------------------------------------------------------
%% Definição do comando FOLHA DE ROSTO
%%------------------------------------------------------------------------------
\newcommand{\folhaDeRosto}{
    \newpage%
    \thispagestyle{empty}%
    \begin{center}
        \Large
        \@autorNome\\
        \@autorDoisNome\\
        \vspace*{3cm} \LARGE \textbf{\@titulo} \\%
    \end{center}
    \vspace*{2cm}
    \hspace{7.5cm}%
    \large%
    \hfill%
    \begin{minipage}{ 8.5cm }%
        \ifthenelse{\boolean{tcc}}%
       {Monografia apresentada como requisito parcial para obtenção do grau de
Bacharel em Ciência da Computação do Instituto de Computação da Universidade
Federal de Alagoas.}%
       {Dissertação apresentada como requisito parcial para obtenção do grau de
Mestre pelo Programa de Pós-Graduação em Modelagem Computacional de Conhecimento do
Instituto de Computação da Universidade Federal de Alagoas.
        \newline\newline
%  	Orientador: \@orientadorNome }%% [Rian] Adicionado href [Marcelo] Adicionado OrientadorDois e Tres
	\ifthenelse{\boolean{orientadorDoisExiste}}{Orientadores:}{Orientador:}%
	\\
        \vspace*{0.2cm}%
        \large
	\href{\@orientadorPagina}{\color{black}\@orientadorNome}\\%
	\href{\@orientadorDoisPagina}{\color{black}\@orientadorDoisNome}\\%
	\@orientadorTresNome\\
	}
\end{minipage}%
    \begin{center}
        \normalsize%
        \vfill%
        Maceió, \@data%
    \end{center}
    \normalsize%
}%

%%------------------------------------------------------------------------------
%% Definição do comando CATALOGACAO por Torres (15/08/2012)
%%------------------------------------------------------------------------------

\newcommand{\catalogacao}{
  \newpage
  \thispagestyle{empty}
  \mbox{}\vfill
  \begin{center}
   {\bf
   {\large
    Catalogação na fonte\\
    Universidade Federal de Alagoas\\
    Biblioteca Central\\
%    Divisão de Tratamento Técnico\\
   }
    Bibliotecária Responsável: Helena Cristina Pimentel do Vale \\
   }
  \framebox[14.5cm][l]{
   \parbox{14.4cm}{
    \mbox{}\newline
    O48t\hspace{.7cm}Oliveira, Marcelo Queiroz de Assis.\newline
    \mbox{}\hspace{1.9cm}\parbox{12.3cm}{
    \begin{flushleft}
      \mbox{}\hspace{.5cm}\@titulo  / Marcelo Queiroz de Assis Oliveira - 2017\\
      \mbox{}\hspace{.5cm} 52 f.\ : il.
    \end{flushleft}
    \begin{flushleft}
      \mbox{}\hspace{.5cm} Orientadores: Alejandro C.\ Frery, Heitor Soares Ramos Filho.\\
      %\mbox{}\hspace{3.0cm} Heitor Soares Ramos Filho.\\
      \mbox{}\hspace{.5cm} Dissertação (Mestrado em Modelagem Computacional de Conhecimento) -- Universidade Federal de Alagoas. Instituto de Computação. Programa de Pós-Graduação em Modelagem Computacional de Conhecimento. Maceió, 2017.
    \end{flushleft}
    \begin{flushleft}
      \mbox{}\hspace{.5cm} Bibliografia: f.\ 50--52.
      \newline\newline
      \mbox{}\hspace{.5cm}1. Teoria da Informação. 2. Geradores de números aleatórios. 3. Processamento de dados - Testes teóricos. 4. Matemática computacional – Testes estatísticos.

    \end{flushleft}
    }
    \begin{flushright}
      CDU: 004.94:519.724\hspace{.5cm}\mbox{}
    \end{flushright}
   }
  }
  \end{center}
\newpage
\setcounter{page}{1}
}%


%%------------------------------------------------------------------------------
%% Definição do comando APROVACAO
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Contador de assinaturas
%%------------------------------------------------------------------------------
\newcounter{numeroDeAssinaturas}
\setcounter{numeroDeAssinaturas}{4}

%%------------------------------------------------------------------------------
%% Início do termo de aprovação
%%------------------------------------------------------------------------------
\newcommand{\aprovacao}{
    \newpage%
    \thispagestyle{empty}%
    \ifthenelse{\boolean{tcc}}%
        {
Monografia apresentada como requisito parcial para obtenção do grau de
Bacharel em Ciência da Computação do Instituto de Computação da Universidade
Federal de Alagoas, aprovada pela comissão examinadora que abaixo assina.}%
        {
 Membros da Comissão Julgadora da Dissertação de Mestrado de 
 Marcelo Queiroz de Assis Oliveira, intitulada ``Teste para Verificação da Hipótese de Ruído Branco utilizando Teoria da Informação'', 
 apresentada ao Programa de Pós-Graduação em Modelagem Computacional de Conhecimento 
 da Universidade Federal de Alagoas em 09 de novembro de 2017, às 9h30min, 
 na sala de aula do Mestrado em Modelagem Computacional de Conhecimento.
}%
         {
 Dissertação apresentada como requisito parcial para obtenção do grau de
 Mestre pelo Curso de Mestrado em Modelagem Computacional de Conhecimento do
 Instituto de Computação da Universidade Federal de Alagoas, aprovada pela
 comissão examinadora que abaixo assina.
 }%

     \begin{center}
         \normalsize%
         % 6 assinaturas: 0.7cm
         % 5 assinaturas: 1.5cm
         % 4 assinaturas: 3.0cm
         % 3 assinaturas: 4.0cm
         % 2 assinaturas: 4.0cm
         \ifthenelse{\value{numeroDeAssinaturas} = 2}{\vspace*{4cm}}{}%
         \ifthenelse{\value{numeroDeAssinaturas} = 3}{\vspace*{4cm}}{}%
         \ifthenelse{\value{numeroDeAssinaturas} = 4}{\vspace*{3cm}}{}%
         \ifthenelse{\value{numeroDeAssinaturas} = 5}{\vspace*{1.5cm}}{}%
         \ifthenelse{\value{numeroDeAssinaturas} = 6}{\vspace*{0.7cm}}{}%
         \rule{9cm}{0.02cm}\\%
         \vspace*{0.2cm}%
         \@orientadorNome~--~Orientador\\
         \@orientadorDepartamento\\
         \@orientadorInstituicao\\
         \ifthenelse{\boolean{orientadorDoisExiste}}{
             \vspace*{1cm}%
             \rule{9cm}{0.02cm}\\%
             \vspace*{0.2cm}%
             \@orientadorDoisNome~--~Orientador\\
             \@orientadorDoisDepartamento\\
             \@orientadorDoisInstituicao\\
         }{}
         \ifthenelse{\boolean{orientadorTresExiste}}{
             \vspace*{1cm}%
             \rule{9cm}{0.02cm}\\%
             \vspace*{0.2cm}%
             \@orientadorTresNome~--~Orientador\\
             \@orientadorTresDepartamento\\
             \@orientadorTresInstituicao\\
         }{}
         \vspace*{1cm}%
         \rule{9cm}{0.02cm}\\%
         \vspace*{0.2cm}%
         \@examinadorNome~--~Examinador\\
         \@examinadorDepartamento\\
         \@examinadorInstituicao\\
         \ifthenelse{\boolean{examinadorDoisExiste}}{
             \vspace*{1cm}%
             \rule{9cm}{0.02cm}\\%
             \vspace*{0.2cm}%
             \@examinadorDoisNome~--~Examinador\\
             \@examinadorDoisDepartamento\\
             \@examinadorDoisInstituicao\\
         }{}
         \ifthenelse{\boolean{examinadorTresExiste}}{
             \vspace*{1cm}%
             \rule{9cm}{0.02cm}\\%
             \vspace*{0.2cm}%
             \@examinadorTresNome~--~Examinador\\
             \@examinadorTresDepartamento\\
             \@examinadorTresInstituicao\\
         }{}
         \ifthenelse{\boolean{examinadorQuatroExiste}}{
             \vspace*{1cm}%
             \rule{9cm}{0.02cm}\\%
             \vspace*{0.2cm}%
             \@examinadorQuatroNome~--~Examinador\\
             \@examinadorQuatroDepartamento\\
             \@examinadorQuatroInstituicao\\
         }{}
         \vfill%
         Maceió, \@data%
     \end{center}
    \normalsize%
    

%     \begin{changemargin}{-3cm}{-2cm}
%       \begin{center}
%%          \includegraphics[width=1\linewidth]{capitulos/FolhaAprovacao.eps}
%       \end{center}
%     \end{changemargin}
% 

}

%%------------------------------------------------------------------------------
%% Definição do comando INICIO para formatação de páginas
%%------------------------------------------------------------------------------
\newcommand{\inicio}{%
    \newpage%
    %Números no estilo arábico
    \renewcommand{\thepage}{\arabic{page}}%
    %Contador de página = 1
    \setcounter{page}{1}%
    %Espaçamento 1,5
    \onehalfspacing%
}%

%%------------------------------------------------------------------------------
%% Definição do comando AUTOR
%%------------------------------------------------------------------------------
\newcommand{\@autorNome}{}
\newcommand{\@autorEmail}{}
\newcommand{\@autorPagina}{}
\newcommand{\autor}[3]{
    \renewcommand{\@autorNome}{#1}
    \renewcommand{\@autorEmail}{#2}
    \renewcommand{\@autorPagina}{#3} % [Rian] página Web do autor
}

%%------------------------------------------------------------------------------
%% Definição do comando AUTOR2
%%------------------------------------------------------------------------------
\newcommand{\@autorDoisNome}{~}
\newcommand{\@autorDoisEmail}{~}
\newboolean{autorDoisExiste}
\setboolean{autorDoisExiste}{false}
\newcommand{\autorDois}[2]{
    \renewcommand{\@autorDoisNome}{#1}
    \renewcommand{\@autorDoisEmail}{#2}
    \setboolean{autorDoisExiste}{true}
}

%%------------------------------------------------------------------------------
%% Definição do comando ORIENTADOR
%%------------------------------------------------------------------------------
\newcommand{\@orientadorNome}{}
\newcommand{\@orientadorPagina}{}
\newcommand{\@orientadorDepartamento}{}
\newcommand{\@orientadorInstituicao}{}
\newcommand{\orientador}[4]{
    \renewcommand{\@orientadorNome}{#1}
    \renewcommand{\@orientadorPagina}{#2} % [Rian] página Web do orientador
    \renewcommand{\@orientadorDepartamento}{#3}
    \renewcommand{\@orientadorInstituicao}{#4}
}

%%------------------------------------------------------------------------------
%% Definição do comando ORIENTADOR 2
%%------------------------------------------------------------------------------
\newcommand{\@orientadorDoisNome}{~}
\newcommand{\@orientadorDoisPagina}{~}
\newcommand{\@orientadorDoisDepartamento}{~}
\newcommand{\@orientadorDoisInstituicao}{~}
\newboolean{orientadorDoisExiste}
\setboolean{orientadorDoisExiste}{false}
\newcommand{\orientadorDois}[4]{
    \renewcommand{\@orientadorDoisNome}{#1}
    \renewcommand{\@orientadorDoisPagina}{#2} % [Marcelo] página Web do orientadorDois
    \renewcommand{\@orientadorDoisDepartamento}{#3}
    \renewcommand{\@orientadorDoisInstituicao}{#4}
    \setboolean{orientadorDoisExiste}{true}
    \addtocounter{numeroDeAssinaturas}{1}
}

%%------------------------------------------------------------------------------
%% Definição do comando ORIENTADOR 3
%%------------------------------------------------------------------------------
\newcommand{\@orientadorTresNome}{~}
\newcommand{\@orientadorTresEmail}{~}
\newcommand{\@orientadorTresDepartamento}{~}
\newcommand{\@orientadorTresInstituicao}{~}
\newboolean{orientadorTresExiste}
\setboolean{orientadorTresExiste}{false}
\newcommand{\orientadorTres}[4]{
    \renewcommand{\@orientadorTresNome}{#1}
    \renewcommand{\@orientadorTresEmail}{#2}
    \renewcommand{\@orientadorTresDepartamento}{#3}
    \renewcommand{\@orientadorTresInstituicao}{#4}
    \setboolean{orientadorTresExiste}{true}
    \addtocounter{numeroDeAssinaturas}{1}
}

%%------------------------------------------------------------------------------
%% Definição do comando EXAMINADOR
%%------------------------------------------------------------------------------
\newcommand{\@examinadorNome}{}
\newcommand{\@examinadorEmail}{}
\newcommand{\@examinadorDepartamento}{}
\newcommand{\@examinadorInstituicao}{}
\newcommand{\examinador}[4]{
    \renewcommand{\@examinadorNome}{#1}
    \renewcommand{\@examinadorEmail}{#2}
    \renewcommand{\@examinadorDepartamento}{#3}
    \renewcommand{\@examinadorInstituicao}{#4}
}

%%------------------------------------------------------------------------------
%% Definição do comando EXAMINADOR 2
%%------------------------------------------------------------------------------
\newcommand{\@examinadorDoisNome}{~}
\newcommand{\@examinadorDoisEmail}{~}
\newcommand{\@examinadorDoisDepartamento}{~}
\newcommand{\@examinadorDoisInstituicao}{~}
\newboolean{examinadorDoisExiste}
\setboolean{examinadorDoisExiste}{false}
\newcommand{\examinadorDois}[4]{
    \renewcommand{\@examinadorDoisNome}{#1}
    \renewcommand{\@examinadorDoisEmail}{#2}
    \renewcommand{\@examinadorDoisDepartamento}{#3}
    \renewcommand{\@examinadorDoisInstituicao}{#4}
    \setboolean{examinadorDoisExiste}{true}
    \addtocounter{numeroDeAssinaturas}{1}
}

%%------------------------------------------------------------------------------
%% Definição do comando EXAMINADOR 3
%%------------------------------------------------------------------------------
\newcommand{\@examinadorTresNome}{~}
\newcommand{\@examinadorTresEmail}{~}
\newcommand{\@examinadorTresDepartamento}{~}
\newcommand{\@examinadorTresInstituicao}{~}
\newboolean{examinadorTresExiste}
\setboolean{examinadorTresExiste}{false}
\newcommand{\examinadorTres}[4]{
    \renewcommand{\@examinadorTresNome}{#1}
    \renewcommand{\@examinadorTresEmail}{#2}
    \renewcommand{\@examinadorTresDepartamento}{#3}
    \renewcommand{\@examinadorTresInstituicao}{#4}
    \setboolean{examinadorTresExiste}{true}
    \addtocounter{numeroDeAssinaturas}{1}
}

%%------------------------------------------------------------------------------
%% Definição do comando EXAMINADOR 4
%%------------------------------------------------------------------------------
\newcommand{\@examinadorQuatroNome}{~}
\newcommand{\@examinadorQuatroEmail}{~}
\newcommand{\@examinadorQuatroDepartamento}{~}
\newcommand{\@examinadorQuatroInstituicao}{~}
\newboolean{examinadorQuatroExiste}
\setboolean{examinadorQuatroExiste}{false}
\newcommand{\examinadorQuatro}[4]{
    \renewcommand{\@examinadorQuatroNome}{#1}
    \renewcommand{\@examinadorQuatroEmail}{#2}
    \renewcommand{\@examinadorQuatroDepartamento}{#3}
    \renewcommand{\@examinadorQuatroInstituicao}{#4}
    \setboolean{examinadorQuatroExiste}{false}
    \addtocounter{numeroDeAssinaturas}{1}
}

%%------------------------------------------------------------------------------
%% Definição do comando TITULO
%%------------------------------------------------------------------------------
\newcommand{\@titulo}{}
\newcommand{\titulo}[1]{\renewcommand{\@titulo}{#1}}

%%------------------------------------------------------------------------------
%% Definição do comando DATA
%%------------------------------------------------------------------------------
\newcommand{\@data}{}
\newcommand{\dataMesAno}[3]{
    \renewcommand{\@data}{
        #1 de #2
    }%%Fim de @DATA
}%Fim de DATA

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       DEFINIÇÃO DE AMBIENTES                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% [Torres] Início Modificações %%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------------------------------------------------------
%% Definição do ambiente RESUMO
%%------------------------------------------------------------------------------
\newenvironment{resumo}{
    \onehalfspacing
    \chapter*{\vspace{-3cm}\centering \Large \textsc{Resumo}}%
    \thispagestyle{empty}%
    \singlespacing%
}{}

%%------------------------------------------------------------------------------
%% Redefinição do ambiente ABSTRACT
%%------------------------------------------------------------------------------
\renewenvironment{abstract}{
    \onehalfspacing
    \chapter*{\vspace{-3cm}\centering \Large \textsc{Abstract}}%
    \thispagestyle{empty}%
    \singlespacing%
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente AGRADECIMENTOS
%%------------------------------------------------------------------------------
\newenvironment{agradecimentos}{
    \onehalfspacing
    \chapter*{\vspace{-3cm}\centering \Large \textsc{Agradecimentos}}
    \thispagestyle{empty}
    \vspace{.4cm}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente LISTA DE FIGURAS
%%------------------------------------------------------------------------------
\newenvironment{mylistfigure}{
    \chapter*{\vspace{-2.8cm}\centering \Large \textsc{Lista de Figuras}}
    \renewcommand{\listfigurename}{}
    \vspace{-2.66cm}
    \cftsetindents{figure}{0em}{2.5em}
    \listoffigures
    \thispagestyle{empty}
%     \addcontentsline{toc}{table}{Lista de Figuras}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente LISTA DE FIGURAS
%%------------------------------------------------------------------------------
\newenvironment{mylistoffigure}{
    \chapter*{\vspace{-2.8cm}\centering \Large \textsc{Lista de Figuras}}
    \renewcommand{\listfigurename}{}
    \vspace{-2.66cm}
    \cftsetindents{figure}{0em}{2.5em}
    \listoffigures
    \thispagestyle{empty}
%     \addcontentsline{toc}{table}{Lista de Figuras}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente LISTA DE TABELAS
%%------------------------------------------------------------------------------
\newenvironment{mylistoftable}{
    \chapter*{\vspace{-2.8cm}\centering \Large \textsc{Lista de Tabelas}}
    \renewcommand{\listtablename}{}
    \vspace{-2.66cm}
    \cftsetindents{table}{0em}{2.5em}
    \listoftables
    \thispagestyle{empty}
%     \addcontentsline{toc}{table}{Lista de Tabelas}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente LISTA DE ALGORITMOS
%%------------------------------------------------------------------------------
\newenvironment{mylistofalgorithm}{
    \chapter*{\vspace{-2.8cm}\centering \Large Lista de Algoritmos}
    \renewcommand{\listalgorithmname}{}
    \vspace{-2.66cm}
    \listofalgorithms
    \thispagestyle{empty}
%     \addcontentsline{toc}{table}{Lista de Algoritmos}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente LISTA DE EQUAÇÕES
%%------------------------------------------------------------------------------
\newenvironment{mylistofequation}{
    \chapter*{\vspace{-2.8cm}\centering \Large \textsc{Lista de Equações}}
    \thispagestyle{empty}
    \vspace{.5cm}
    \makeatletter \@starttoc{loe} \makeatother
    \newpage\lhead{\rightmark}
%     \addcontentsline{toc}{table}{Lista de Equações}
}{}

%%------------------------------------------------------------------------------
%% Definição do ambiente SUMÁRIO
%%------------------------------------------------------------------------------
\newenvironment{mylistofcontents}{
    \chapter*{\vspace{-2.8cm}\centering \Large \textsc{Sumário}}
    \renewcommand{\contentsname}{}
    \vspace{-3.08cm}
    \cftsetindents{section}{0em}{2.5em}
    \cftsetindents{subsection}{0em}{2.5em}
    \tableofcontents
    \thispagestyle{empty}
}{}
%%%%%%%%%%%%%%%%%%%%%%%%%% [Torres] Fim Modificações %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------------------------------------------
%% Definição do ambiente CODIGO
%%------------------------------------------------------------------------------

% Ambiente COPIADO do pacote listings.sty (Ambiente:lstlisting)
\lstnewenvironment{codigo}[2][]
    {\lst@TestEOLChar{#2}
     \lstset{#1}
     \csname\@lst @SetFirstNumber\endcsname}
    {\csname\@lst @SaveFirstNumber\endcsname}

% Configurações de exibição
\lstset{basicstyle=\small\tt,keywordstyle=\tt,numbers=left,  language=R, frame=trbl}
\lstset{commentstyle=\textit}
\lstset{emph={kappa,var,args,length,mean,set,as,real,file,paste,write,table,commandArgs,print}}
\renewcommand{\lstlistingname}{Lista de Códigos}
\renewcommand{\lstlistingname}{Algoritmo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                 CONFIGURAÇÕES PARA O CORPO DO DOCUMENTO                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\AtBeginDocument{
    \normalsize
    %% Configura para utilizar o cabeçalho e rodapé definidos anteriormente.    
    \pagestyle{fancy}
    %% Define o módulo de citação abreviado(Harvard)
    %% \citationmode{abbr}
    %% Define o estilo bibligráfico agsm(Harvard)
    \bibliographystyle{agsm_url}
}

%%------------------------------------------------------------------------------
%% Desabilita a utilização de arroba
%%------------------------------------------------------------------------------
\makeatother
