\documentclass[a4paper]{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage[sfdefault, book, lf]{FiraSans} % lf - lined numbers
\usepackage[colorlinks=true]{hyperref}
\usepackage{graphicx}
\usepackage{cp2223t}
\usepackage{subcaption}
\usepackage{adjustbox}
\usepackage[indent=12pt]{parskip}

%================= lhs2tex=====================================================%
%include polycode.fmt
%format (div (x)(y)) = x "\div " y
%format succ = "\succ "
%format ==> = "\Longrightarrow "
%format map = "\map "
%format length = "\length "
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format . = "\comp "
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format -|- = "+"
%format conc = "\mathsf{conc}"
%format summation = "{\sum}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (uncurry f) = "\uncurry{" f "}"
%format (const (f)) = "\underline{" f "}"
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (lcbr3 (x)(y)(z)) = "\begin{lcbr}" x "\\" y "\\" z "\end{lcbr}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format Either a b = a "+" b
%format fmap = "\mathsf{fmap}"
%format NA   = "\textsc{na}"
%format NB   = "\textbf{NB}"
%format inT = "\mathsf{in}"
%format outT = "\mathsf{out}"
%format outLTree = "\mathsf{out}_{\tiny\ \textit{LTree}}"
%format inLTree = "\mathsf{in}_{\tiny\ \textit{LTree}}"
%format inFTree = "\mathsf{in}_{\tiny\ \textit{FTree}}"
%format outFTree = "\mathsf{out}_{\tiny\ \textit{FTree}}"
%format inExp = "\mathsf{in}_{\tiny\ \textit{Exp}}"
%format outExp = "\mathsf{out}_{\tiny\ \textit{Exp}}"
%format Null = "1"
%format (Prod (a) (b)) = a >< b
%format fF = "\fun F "
%format l2 = "l_2 "
%format Dist = "\fun{Dist}"
%format IO = "\fun{IO}"
%format LTree = "{\LTree}"
%format FTree = "{\FTree}"
%format inNat = "\mathsf{in}"
%format (cata (f)) = "\llparenthesis\, " f "\,\rrparenthesis"
%format (cataNat (g)) = "\llparenthesis\, " g "\,\rrparenthesis"
%format (cataList (g)) = "\llparenthesis\, " g "\,\rrparenthesis"
%format (cataLTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataFTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataRose (x)) = "\llparenthesis\, " x "\,\rrparenthesis_\textit{\tiny R}"
%format (cataExp (x)) = "\llparenthesis\, " x "\,\rrparenthesis_\textit{\tiny Exp}"
%format (ana (g)) = "\ana{" g "}"
%format (anaList (g)) = "\anaList{" g "}"
%format (anaLTree (g)) = "\lanabracket\;\!" g "\;\!\ranabracket"
%format (anaRose (g)) = "\lanabracket\;\!" g "\;\!\ranabracket_\textit{\tiny R}"
%format (anaExp (g)) = "\lanabracket\;\!" g "\;\!\ranabracket_\textit{\tiny Exp}"
%format (hylo (g) (h)) = "\llbracket\, " g ",\," h "\,\rrbracket"
%format (hyloRose (g) (h)) = "\llbracket\, " g ",\," h "\,\rrbracket_\textit{\tiny R}"
%format (hyloExp (g) (h)) = "\llbracket\, " g ",\," h "\,\rrbracket_\textit{\tiny Exp}"
%format Nat0 = "\N_0"
%format Rational = "\Q "
%format toRational = " to_\Q "
%format fromRational = " from_\Q "
%format muB = "\mu "
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (fac (n)) = "{" n "!}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format matrix = "matrix "
%format `ominus` = "\mathbin{\ominus}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format `minusNat`= "\mathbin{-}"
%format ==> = "\Rightarrow"
%format .==>. = "\Rightarrow"
%format .<==>. = "\Leftrightarrow"
%format .==. = "\equiv"
%format .<=. = "\leq"
%format .&&&. = "\wedge"
%format cdots = "\cdots "
%format pi = "\pi "
%format (curry (f)) = "\overline{" f "}"
%format delta = "\Delta "
%format (plus (f)(g)) = "{" f "}\plus{" g "}"
%format ++ = "\mathbin{+\!\!\!+}"
%format Integer  = "\mathbb{Z}"
%format (Cp.cond (p) (f) (g)) = "\mcond{" p "}{" f "}{" g "}"
\def\plus{\mathbin{\dagger}}
%format square (x) = x "^2"
%format a1 = "a_1 "	
%format a2 = "a_2 "	
%format a3 = "a_3 "	
%format a4 = "a_4 "	
%format b1 = "b_1 "	
%format b2 = "b_2 "	
%format b3 = "b_3 "	
%format b4 = "b_4 "	
%format c1 = "c_1 "	
%format c2 = "c_2 "	
%format c3 = "c_3 "	
%format c4 = "c_4 "	
%format d1 = "d_1 "	
%format d2 = "d_2 "	
%format d3 = "d_3 "	
%format d4 = "d_4 "	
%format r1 = "r_1 "	
%format r2 = "r_2 "	
%format s1 = "s_1 "	
%format s2 = "s_2 "	
%format e1 = "e_1 "	
%format e2 = "e_2 "	
\def\kcomp{\mathbin{\bullet}}
%format (kcomp (f) (g)) = f "\kcomp " g
%format .! = "\kcomp"
%---------------------------------------------------------------------------

\title{
          \textbf{Cálculo de Programas}
\\
          Trabalho Prático
\\
          LEI --- 2022/23
}

\author{
          \dium
\\
          Universidade do Minho
}


\date\mydate

\makeindex
\newcommand{\rn}[1]{\textcolor{Red}{#1}}
\begin{document}
\emergencystretch 3em
%\sloppy

\maketitle

\begin{center}\large
\begin{tabular}{ll}
Grupo nr. & 05
\\\hline
a96215 & João Martins
\\
a97541 & Gonçalo Braga
\\
a95019 & João Gonçalves
\end{tabular}
\end{center}

\section*{Preâmbulo}

\CP\ tem como objectivo principal ensinar
a progra\-mação de computadores como uma disciplina científica. Para isso
parte-se de um repertório de \emph{combinadores} que formam uma álgebra da
programação (conjunto de leis universais e seus corolários) e usam-se esses
combinadores para construir programas \emph{composicionalmente}, isto é,
agregando programas já existentes.

Na sequência pedagógica dos planos de estudo dos cursos que têm
esta disciplina, opta-se pela aplicação deste método à programação
em \Haskell\ (sem prejuízo da sua aplicação a outras linguagens
funcionais). Assim, o presente trabalho prático coloca os
alunos perante problemas concretos que deverão ser implementados em
\Haskell.  Há ainda um outro objectivo: o de ensinar a documentar
programas, a validá-los e a produzir textos técnico-científicos de
qualidade.

Antes de abodarem os problemas propostos no trabalho, os grupos devem ler
com atenção o anexo \ref{sec:documentacao} onde encontrarão as instruções
relativas ao sofware a instalar, etc.

%if False
\begin{code}
{-# OPTIONS_GHC -XNPlusKPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module Main where
import Cp
import List hiding (fac)
import NEList (out)
import Exp
import Nat hiding (aux)
import LTree
import Rose hiding (g)
import Probability
import Data.List hiding (find)
import Data.List.Split hiding (split,chunksOf) 
import Svg hiding (for,wrap)
import Control.Concurrent
import Cp2223data
import Control.Monad (when)
import qualified BTree

main = undefined
instance Strong Dist
\end{code}
%endif

\Problema
Suponha-se uma sequência numérica semelhante à sequência de Fibonacci tal
que cada termo subsequente aos três primeiros corresponde à soma dos três
anteriores, sujeitos aos coeficientes |a|, |b| e |c|:
\begin{code}
f a b c 0 = 0
f a b c 1 = 1
f a b c 2 = 1
f a b c (n+3) = a * f a b c (n+2) + b * f a b c (n+1) + c * f a b c n
\end{code}
Assim, por exemplo, |f 1 1 1| irá dar como resultado a sequência:
\begin{spec}
1, 1, 2, 4, 7, 13, 24, 44, 81, 149, ...
\end{spec}
|f 1 2 3| irá gerar a sequência:
\begin{spec}
1,1,3,8,17,42,100,235,561,1331, ...
\end{spec}
etc.

A definição de |f| dada é muito ineficiente, tendo uma degradação do tempo
de execução exponencial.
Pretende-se otimizar a função dada convertendo-a para um ciclo \textit{for}.
Recorrendo à lei de recursividade mútua, calcule |loop| e |initial| em
\begin{code}
fbl a b c = wrap . (for ((loop a b c)) initial)
\end{code}
por forma a |f| e |fbl| serem (matematicamente) a mesma função. 
Para tal, poderá usar a regra prática explicada no anexo \ref{sec:mr}.

\textbf{Valorização}: apresente testes de \textit{performance} que mostrem
quão mais rápida é |fbl| quando comparada com |f|.

\Problema
Pretende-se vir a classificar os conteúdos programáticos de todas as
\href{https://web.di.uminho.pt/sitedi/ucs/}{UCs} lecionadas no \dium\ de
acordo com o \href{https://dl.acm.org/ccs}{ACM Computing Classification System}.
A listagem da taxonomia desse sistema está disponível no ficheiro
\texttt{Cp2223data}, 
começando com
\begin{spec}
acm_ccs = [  "CCS",
             "    General and reference",
             "        Document types",
             "            Surveys and overviews",
             "            Reference works",
             "            General conference proceedings",
             "            Biographies",
             "            General literature",
             "            Computing standards, RFCs and guidelines",
             "        Cross-computing tools and techniques",
\end{spec}
(10 primeiros ítens) etc., etc.\footnote{Informação obtida a partir do site
\href{https://dl.acm.org/ccs}{ACM CCS} selecionando \emph{Flat View}.}

Pretende-se representar a mesma informação sob a forma de uma árvore de expressão,
usando para isso a biblioteca \Exp\ que consta do material padagógico da disciplina e
que vai incluída no zip do projecto, por  ser mais conveniente para os alunos.

\begin{enumerate}
\item Comece por definir a função de conversão do texto dado em |acm_ccs|
(uma lista de \emph{strings}) para uma tal árvore como um anamorfismo de \Exp:
%
\begin{code}
tax :: [String] -> Exp String String
tax = anaExp gene
\end{code}
Ou seja, defina o |gene| do anamorfismo, 
tendo em conta o seguinte diagrama\footnote{|S| abrevia |String|.}:
\begin{eqnarray*}
\xymatrix{
  |Exp S S| & & S + S \times (|Exp S S|)^*\ar[ll]_{|inExp|} \\
  S^*\ar@@/_1.5pc/[rr]_{|gene|}\ar[r]^(0.35){|out|}\ar[u]^{|tax|} & S + S \times S^*\ar[r]^(0.45){\cdots} & S + S \times (S^*)^*\ar[u]_{id + id \times tax^*}
}
\end{eqnarray*}
Para isso, tome em atenção que cada nível da hierarquia é, em |acm_ccs|,
marcado pela indentação de 4 espaços adicionais --- como se mostra no fragmento acima.

Na figura \ref{fig:P1} mostra-se a representação gráfica da árvore de tipo \Exp\ que representa o fragmento de |acm_ccs| mostrado acima.

\begin{figure}[ht!]
\centering
\begin{tikzpicture}
[-,every node/.style={shape=rectangle,inner sep=3pt,draw}]
\footnotesize
\node {CSS} [edge from parent fork down]
  [sibling distance=4cm]
  child {node [align=center] {General and\\reference}
    [sibling distance=4cm]
    child {node {Document types}
      [sibling distance=2.25cm]
      child {node [align=center] {Surveys and\\overviews}}
      child {node [align=center] {Reference\\works}}
      child {node [align=center] {General\\conference\\proceedings}}
      child {node [align=center] {Biographies}}
      child {node [align=center] {General\\literature}}
      child {node [align=center, xshift=0.75cm] {Computing standards,\\RFCs and\\guidelines}}
    }
    child {node [align=center] {Cross-computing tools and\\techniques}}
  }
  ;
\end{tikzpicture}
\caption{Fragmento de |acm_ccs| representado sob a forma de uma árvore do tipo \Exp.}
\label{fig:P1}
\end{figure}

\item De seguida vamos querer todos os caminhos da árvore que é gerada por |tax|,
pois a classificação de uma UC pode ser feita a qualquer nível (isto é, caminho
descendente da raiz |"CCS"| até um subnível ou folha).
\footnote{Para um exemplo de classificação de UC concreto, pf.\  ver a secção \textbf{Classificação ACM} na página
pública de \CP.}

Precisamos pois da composição de |tax| com uma função de pós-processamento |post|,
%
\begin{spec}
tudo :: [String] -> [[String]]
tudo = post . tax
\end{spec}
para obter o efeito que se mostra na tabela \ref{table:acmccs}.

\begin{table}[ht!]
\centering\small
\begin{center}
\begin{tabular}{||l||l||l||l||}
\hline
CCS & & & 
\\\hline
CCS & General and reference & & 
\\\hline
CCS & General and reference & Document types & 
\\\hline
CCS & General and reference & Document types & Surveys and overviews
\\\hline
CCS & General and reference & Document types & Reference works
\\\hline
CCS & General and reference & Document types & General conference proceedings
\\\hline
CCS & General and reference & Document types & Biographies
\\\hline
CCS & General and reference & Document types & General literature
\\\hline
CCS & General and reference & Cross-computing tools and techniques & 
\\\hline
\end{tabular}
\end{center}
\caption{Taxonomia ACM fechada por prefixos (10 primeiros ítens).}
\label{table:acmccs}
\end{table}

Defina a função |post :: Exp String String -> [[String]]| da forma mais económica que encontrar.
\end{enumerate}

\textbf{Sugestão}: Inspecione as bibliotecas fornecidas à procura de funções
auxiliares que possa re-utilizar para a sua solução ficar mais simples.
Não se esqueça que, para o mesmo resultado,
nesta disciplina \emph{``ganha'' quem escrever menos código}!

\textbf{Sugestão}: Para efeitos de testes intermédios não use a totalidade de |acm_ccs|,
que tem 2114 linhas! Use, por exemplo, |take 10 acm_ccs|, como se mostrou acima.

\Problema

O \sierpCarpet{tapete de Sierpinski} é uma figura geométrica \fractal\ em que um quadrado é subdividido recursivamente em sub-quadrados. A construção clássica do tapete de Sierpinski é a seguinte: assumindo um quadrado de lado |l|, este é subdivido em 9 quadrados iguais de lado |l / 3|, removendo-se o quadrado central. Este passo é depois repetido sucessivamente para cada um dos 8 sub-quadrados restantes (Fig.~\ref{fig:sierp1}).

\begin{figure}[h!]
  \centering
  \includegraphics[width=0.19\textwidth]{cp2223t_media/tapete1.png}
  \includegraphics[width=0.19\textwidth]{cp2223t_media/tapete2.png}
  \includegraphics[width=0.19\textwidth]{cp2223t_media/tapete3.png}
  \includegraphics[width=0.19\textwidth]{cp2223t_media/tapete4.png}
  \includegraphics[width=0.19\textwidth]{cp2223t_media/tapete5.png}
  \caption{Construção do tapete de Sierpinski com profundidade 5.}
  \label{fig:sierp1}
\end{figure}

\noindent
|NB|: No exemplo da fig.~\ref{fig:sierp1}, assumindo a construção clássica já referida, os quadrados estão a branco e o fundo a verde.

A complexidade deste algoritmo, em função do número de quadrados a desenhar, para uma profundidade $n$, é de $8^n$ (exponencial). No entanto, se assumirmos que os quadrados a desenhar são os que estão a verde, a complexidade é reduzida para $\sum_{i=0}^{n-1}8^i$, obtendo um ganho de $\sum_{i=1}^{n} \frac{100}{8^i} \%$. Por exemplo, para $n=5$, o ganho é de $14.28 \%$. O objetivo deste problema é a implementação do algoritmo mediante a referida otimização.
\begin{figure}[h!]
  \centering
  \includegraphics[width=0.35\textwidth]{cp2223t_media/tapete_ex}
  \caption{Tapete de Sierpinski com profundidade 2 e com os quadrados enumerados.}
  \label{fig:sierp2}
\end{figure}

Assim, seja cada quadrado descrito geometricamente pelas coordenadas do seu vértice inferior esquerdo e o comprimento do seu lado:
\begin{code}
type Square = (Point, Side)
type Side = Double
type Point = (Double, Double)
\end{code}
A estrutura recursiva de suporte à construção de tapetes de Sierpinski será uma \Rose, na qual cada nível da árvore irá guardar os quadrados de tamanho igual. Por exemplo, a construção da fig.~\ref{fig:sierp2} poderá\footnote{A ordem dos filhos não é relevante.} corresponder à árvore da figura \ref{fig:roseTreeSierp}.
\begin{figure}[ht!]
\centering
\begin{tikzpicture}
[level distance = 2cm,
level 1/.style = {sibling distance = 1.5cm},
level 2/.style = {sibling distance = 0.9cm},
]\node [draw, circle]{1}
child {node [draw, circle]{2}
child {node [draw, circle]{10}}
child {node [draw, circle]{11}}
child {node [draw, circle]{12}}
child {node [draw, circle]{13}}
child {node [draw, circle]{14}}
child {node [draw, circle]{15}}
child {node [draw, circle]{16}}
child {node [draw, circle]{17}}}
child {node [draw, circle]{3}}
child {node [draw, circle]{4}}
child {node [draw, circle]{5}}
child {node [draw, circle]{6}}
child {node [draw, circle]{7}}
child {node [draw, circle]{8}}
child {node [draw, circle]{9}};
\end{tikzpicture}
\caption{Possível árvore de suporte para a construção da fig.~\ref{fig:sierp2}.}
\label{fig:roseTreeSierp}
\end{figure}

Uma vez que o tapete é também um quadrado, o objetivo será, a partir das informações do tapete (coordenadas do vértice inferior esquerdo e comprimento do lado), desenhar o quadrado central, subdividir o tapete nos 8 sub-tapetes restantes, e voltar a desenhar, recursivamente, o quadrado nesses 8 sub-tapetes. Desta forma, cada tapete determina o seu quadrado e os seus 8 sub-tapetes. No exemplo em cima, o tapete que contém o quadrado 1 determina esse próprio quadrado e determina os sub-tapetes que contêm os quadrados 2 a 9.

Portanto, numa primeira fase, dadas as informações do tapete, é construida a árvore de suporte com todos os quadrados a desenhar, para uma determinada profundidade.
\begin{code}
squares :: (Square, Int) -> Rose Square
\end{code}
|NB|: No programa, a profundidade começa em $0$ e não em $1$.

Uma vez gerada a árvore com todos os quadrados a desenhar, é necessário extrair os quadrados para uma lista, a qual é processada pela função |drawSq|, disponibilizada no anexo \ref{sec:codigo}.
\begin{code}
rose2List :: Rose a -> [a]
\end{code}
Assim, a construção de tapetes de Sierpinski é dada por um hilomorfismo de \textit{Rose Trees}:
\begin{code}
sierpinski :: (Square, Int) -> [Square]
sierpinski = hyloRose gr2l  gsq
\end{code}
\textbf{Trabalho a fazer:}
\begin{enumerate}
    \item Definir os genes do hilomorfismo |sierpinski|.
    \item Correr
\begin{code}
sierp4 = drawSq (sierpinski (((0,0),32),3))

constructSierp5 = do drawSq (sierpinski (((0,0),32),0))
                     await
                     drawSq (sierpinski (((0,0),32),1))
                     await
                     drawSq (sierpinski (((0,0),32),2))
                     await
                     drawSq (sierpinski (((0,0),32),3))
                     await
                     drawSq (sierpinski (((0,0),32),4))
                     await
\end{code}
     \item Definir a função que apresenta a construção do tapete de Sierpinski como é apresentada em |construcaoSierp5|, mas para uma profundidade $n \in \mathbb{N}$ recebida como parâmetro.
\begin{code}
constructSierp :: Int -> IO [()]
constructSierp = present . carpets
\end{code}
\textbf{Dica}: a função |constructSierp| será um hilomorfismo de listas, cujo anamorfismo |carpets :: Int -> [[Square]]| constrói, recebendo como parâmetro a profundidade $n$, a lista com todos os tapetes de profundidade $1..n$, e o catamorfismo |present :: [[Square]] -> IO [()]| percorre a lista desenhando os tapetes e esperando 1 segundo de intervalo.
\end{enumerate}

\Problema

Este ano ocorrerá a vigésima segunda edição do Campeonato do Mundo de Futebol, organizado pela Federação Internacional de Futebol (FIFA), a decorrer no Qatar e com o jogo inaugural a 20 de Novembro.

Uma casa de apostas pretende calcular, com base numa aproximação dos \textit{rankings}\footnote{Os \textit{rankings} obtidos \href{https://www.fifa.com/fifa-world-ranking/men?dateId=id13687}{aqui} foram escalados e arredondados.} das seleções, a probabilidade de cada seleção vencer a competição.

Para isso, o diretor da casa de apostas contratou o Departamento de Informática da Universidade do Minho, que atribuiu o projeto à equipa formada pelos alunos e pelos docentes de Cálculo de Programas.

\begin{alert}
Para resolver este problema de forma simples, ele será abordado por duas fases:
\begin{enumerate}
\item versão académica sem probabilidades, em que se sabe à partida, num jogo, quem o vai vencer;
\item versão realista com probabilidades usando o mónade \textit{Dist} (distribuições probabilísticas) que vem descrito no anexo \ref{sec:probabilities}.
\end{enumerate}
A primeira versão, mais simples, deverá ajudar a construir a segunda.
\end{alert}

\subsection*{Descrição do problema}

Uma vez garantida a qualificação (já ocorrida), o campeonato consta de duas fases consecutivas no tempo:
\begin{enumerate}
\item fase de grupos;
\item fase eliminatória (ou ``mata-mata'', como é habitual dizer-se no Brasil).
\end{enumerate}

Para a fase de grupos, é feito um sorteio das 32 seleções (o qual já ocorreu para esta competição)
que as coloca em 8 grupos, 4 seleções em cada grupo.
Assim, cada grupo é uma lista de seleções.

Os grupos para o campeonato deste ano são:
\begin{code}
type Team = String
type Group = [Team]

groups :: [Group]
groups = [["Qatar", "Ecuador", "Senegal", "Netherlands"],
          ["England", "Iran", "USA", "Wales"],
          ["Argentina", "Saudi Arabia", "Mexico", "Poland"],
          ["France", "Denmark", "Tunisia", "Australia"],
          ["Spain", "Germany", "Japan", "Costa Rica"],
          ["Belgium", "Canada", "Morocco", "Croatia"],
          ["Brazil", "Serbia", "Switzerland", "Cameroon"],
          ["Portugal", "Ghana", "Uruguay", "Korea Republic"]]
\end{code}
Deste modo, \textit{groups !! 0} corresponde ao grupo A, \textit{groups !! 1} ao grupo B, e assim sucessivamente.
Nesta fase, cada seleção de cada grupo vai defrontar (uma vez) as outras do seu grupo. 

Passam para o ``mata-mata'' as duas seleções que mais pontuarem em cada grupo, obtendo pontos, por cada jogo da fase grupos, da seguinte forma:
\begin{itemize}
\item vitória --- 3 pontos;
\item empate --- 1 ponto;
\item derrota --- 0 pontos.
\end{itemize}
Como se disse, a posição final no grupo irá determinar se uma seleção avança para o ``mata-mata'' e, se avançar, que possíveis jogos terá pela frente, uma vez que a disposição das seleções está desde o início definida para esta última fase, conforme se pode ver na figura \ref{fig:wcup22}.
\begin{figure}[ht]
    \centering
    \includegraphics[scale=0.125]{cp2223t_media/wcup2022.png}
    \caption{O ``mata-mata''}
    \label{fig:wcup22}
\end{figure}

Assim, é necessário calcular os vencedores dos grupos sob uma distribuição probabilística.
Uma vez calculadas as distribuições dos vencedores, é necessário colocá-las nas folhas de uma |LTree| de forma a fazer um \textit{match} com a figura \ref{fig:wcup22}, entrando assim na fase final da competição, o tão esperado ``mata-mata''.
Para avançar nesta fase final da competição (i.e.\ subir na árvore), é preciso ganhar, quem perder é automaticamente eliminado (``mata-mata''). Quando uma seleção vence um jogo, sobe na árvore, quando perde, fica pelo caminho. Isto significa que a seleção vencedora é aquela que vence todos os jogos do ``mata-mata''.

\subsection*{Arquitetura proposta}

A visão composicional da equipa permitiu-lhe perceber desde logo que o problema podia ser dividido, independentemente da versão, probabilística ou não, em duas partes independentes --- a da fase de grupos e a do ``mata-mata''. Assim, duas sub-equipas poderiam trabalhar em paralelo, desde que se garantisse a composicionalidade das partes.
Decidiu-se que os alunos desenvolveriam a parte da fase de grupos e os docentes a do ``mata-mata''.

\subsubsection*{Versão não probabilística}
O resultado final (não probabilístico) é dado pela seguinte função:
\begin{code}
winner :: Team
winner = wcup groups

wcup = knockoutStage . groupStage 
\end{code}
A sub-equipa dos docentes já entregou a sua parte:
\begin{code}
knockoutStage = cataLTree (either id koCriteria)
\end{code}

Considere-se agora a proposta do \textit{team leader} da sub-equipa dos alunos para o desenvolvimento da fase de grupos:

\begin{bquote}
{\slshape

Vamos dividir o processo em 3 partes:
\begin{itemize}
\item gerar os jogos,
\item simular os jogos,
\item preparar o ``mata-mata'' gerando a árvore de jogos dessa fase (fig. \ref{fig:wcup22}).
\end{itemize}
Assim:
\begin{code}
groupStage :: [Group] -> LTree Team
groupStage = initKnockoutStage . simulateGroupStage . genGroupStageMatches
\end{code}

Comecemos então por definir a função |genGroupStageMatches| que gera os jogos da fase de grupos:
\begin{code}
genGroupStageMatches :: [Group] -> [[Match]]
genGroupStageMatches = map generateMatches
\end{code}
onde
\begin{code}
type Match = (Team, Team)
\end{code}
Ora, sabemos que nos foi dada a função
\begin{code}
gsCriteria :: Match -> Maybe Team
\end{code}
que, mediante um certo critério, calcula o resultado de um jogo, retornando |Nothing| em caso de empate, ou a equipa vencedora (sob o construtor |Just|). Assim, precisamos de definir a função
\begin{code}
simulateGroupStage :: [[Match]] -> [[Team]]
simulateGroupStage = map (groupWinners gsCriteria)
\end{code}
que simula a fase de grupos e dá como resultado a lista dos vencedores,
recorrendo à função |groupWinners|:
\begin{code}
groupWinners criteria = best 2 . consolidate . (>>= matchResult criteria)
\end{code}
Aqui está apenas em falta a definição da função |matchResult|.

Por fim, teremos a função |initKnockoutStage| que produzirá a |LTree| que a sub-equipa do ``mata-mata'' precisa, com as devidas posições. Esta será a composição de duas funções:
\begin{code}
initKnockoutStage = anaLTree glt . arrangement
\end{code}
}
\end{bquote}
Trabalho a fazer:
\begin{enumerate}
\item	Definir uma alternativa à função genérica |consolidate| que seja um
catamorfismo de listas:
\begin{code}
consolidate' :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
consolidate' = cataList cgene
\end{code}
\item	Definir a função |matchResult :: (Match -> Maybe Team) -> Match ->
	[(Team, Int)]| que apura os pontos das equipas de um dado jogo.
\item Definir a função genérica |pairup :: Eq b => [b] -> [(b, b)]| em que
	|generateMatches| se baseia.
\item Definir o gene |glt|.
\end{enumerate}

\subsubsection*{Versão probabilística}

Nesta versão, mais realista, |gsCriteria :: Match -> (Maybe Team)| dá lugar a
\begin{code}
pgsCriteria :: Match -> Dist (Maybe Team)
\end{code}
que dá, para cada jogo, a probabilidade de cada equipa vencer ou haver um empate.
Por exemplo, há |50%| de probabilidades de Portugal empatar com a Inglaterra,
\begin{quote}
\begin{verbatim}
pgsCriteria("Portugal","England")
        Nothing  50.0%
 Just "England"  26.7%
Just "Portugal"  23.3%
\end{verbatim}
\end{quote}
etc.

O que é |Dist|? É o mónade que trata de distribuições probabilísticas e que é descrito no
anexo \ref{sec:probabilities}, página \pageref{sec:probabilities} e seguintes. O que há a fazer? Eis o que diz o vosso \emph{team leader}:

\begin{bquote}
\slshape
O que há a fazer nesta versão é, antes de mais, avaliar qual é o impacto
de |gsCriteria| virar monádica (em |Dist|) na arquitetura geral da versão
anterior. Há que reduzir esse impacto ao mínimo, escrevendo-se tão pouco código
quanto possível!
\end{bquote}

Todos relembraram algo que tinham aprendido nas aulas teóricas a respeito
da ``monadificação'' do código: há que reutilizar o código da versão anterior,
monadificando-o.

Para distinguir as duas versões decidiu-se afixar o prefixo `p'  para identificar
uma função que passou a ser monádica.

A sub-equipa dos docentes fez entretanto a monadificação da sua parte:
\begin{spec}
pwinner :: Dist Team
pwinner = pwcup groups

pwcup = pknockoutStage .! pgroupStage 
\end{spec}
E entregou ainda a versão probabilística do ``mata-mata'':
\begin{code}
pknockoutStage = mcataLTree' (either return pkoCriteria)

mcataLTree' g = k where
        k (Leaf a) = g1 a
        k (Fork(x,y)) = mmbin g2 (k x, k y)
        g1 = g . i1
        g2 = g . i2
\end{code}
A sub-equipa dos alunos também já adiantou trabalho,
\begin{code}
pgroupStage = pinitKnockoutStage .! psimulateGroupStage . genGroupStageMatches
\end{code}
mas faltam ainda |pinitKnockoutStage| e |pgroupWinners|, esta usada em |psimulateGroupStage|,
que é dada em anexo. 

Trabalho a fazer:
\begin{itemize}
\item	Definir as funções que ainda não estão implementadas nesta versão.
\item	\textbf{Valorização}: experimentar com outros critérios de ``ranking'' das equipas.
\end{itemize}

\begin{alert}
\textbf{Importante}: (a) código adicional terá que ser colocado no anexo
\ref{sec:resolucao}, obrigatoriamente; (b) todo o código que é dado não pode
ser alterado.
\end{alert}

\part*{Anexos}

\appendix

\section{Documentação para realizar o trabalho}
\label{sec:documentacao}
Para cumprir de forma integrada os objectivos do trabalho vamos recorrer
a uma técnica de programa\-ção dita
``\litp{literária}'' \cite{Kn92}, cujo princípio base é o seguinte:
%
\begin{quote}\em Um programa e a sua documentação devem coincidir.
\end{quote}
%
Por outras palavras, o código fonte e a documentação de um
programa deverão estar no mesmo ficheiro.

O ficheiro \texttt{cp2223t.pdf} que está a ler é já um exemplo de
\litp{programação literária}: foi gerado a partir do texto fonte
\texttt{cp2223t.lhs}\footnote{O sufixo `lhs' quer dizer
\emph{\lhaskell{literate Haskell}}.} que encontrará no
\MaterialPedagogico\ desta disciplina descompactando o ficheiro
\texttt{cp2223t.zip} e executando:
\begin{Verbatim}[fontsize=\small]
    $ lhs2TeX cp2223t.lhs > cp2223t.tex
    $ pdflatex cp2223t
\end{Verbatim}
em que \href{https://hackage.haskell.org/package/lhs2tex}{\texttt\LhsToTeX} é
um pré-processador que faz ``pretty printing''
de código Haskell em \Latex\ e que deve desde já instalar utilizando o
utiliário \href{https://www.haskell.org/cabal/}{cabal} disponível em \href{https://www.haskell.org}{haskell.org}.

Por outro lado, o mesmo ficheiro \texttt{cp2223t.lhs} é executável e contém
o ``kit'' básico, escrito em \Haskell, para realizar o trabalho. Basta executar
\begin{Verbatim}[fontsize=\small]
    $ ghci cp2223t.lhs
\end{Verbatim}

\noindent Abra o ficheiro \texttt{cp2223t.lhs} no seu editor de texto preferido
e verifique que assim é: todo o texto que se encontra dentro do ambiente
\begin{quote}\small\tt
\verb!\begin{code}!
\\ ... \\
\verb!\end{code}!
\end{quote}
é seleccionado pelo \GHCi\ para ser executado.

\subsection{Como realizar o trabalho}
Este trabalho teórico-prático deve ser realizado por grupos de 3 (ou 4) alunos.
Os detalhes da avaliação (datas para submissão do relatório e sua defesa
oral) são os que forem publicados na \cp{página da disciplina} na \emph{internet}.

Recomenda-se uma abordagem participativa dos membros do grupo
em todos os exercícios do trabalho, para assim
poderem responder a qualquer questão colocada na
\emph{defesa oral} do relatório.

Em que consiste, então, o \emph{relatório} a que se refere o parágrafo anterior?
É a edição do texto que está a ser lido, preenchendo o anexo \ref{sec:resolucao}
com as respostas. O relatório deverá conter ainda a identificação dos membros
do grupo de trabalho, no local respectivo da folha de rosto.

Para gerar o PDF integral do relatório deve-se ainda correr os comando seguintes,
que actualizam a bibliografia (com \Bibtex) e o índice remissivo (com \Makeindex),
\begin{Verbatim}[fontsize=\small]
    $ bibtex cp2223t.aux
    $ makeindex cp2223t.idx
\end{Verbatim}
e recompilar o texto como acima se indicou.

No anexo \ref{sec:codigo}, disponibiliza-se algum
código \Haskell\ relativo aos problemas apresentados. Esse anexo deverá
ser consultado e analisado à medida que isso for necessário.

\subsection{Como exprimir cálculos e diagramas em LaTeX/lhs2tex}
Como primeiro exemplo, estudar o texto fonte deste trabalho para obter o
efeito:\footnote{Exemplos tirados de \cite{Ol18}.}
\begin{eqnarray*}
\start
     |id = split f g|
%
\just\equiv{ universal property }
%
        |lcbr(
          p1 . id = f
     )(
          p2 . id = g
     )|
%
\just\equiv{ identity }
%
        |lcbr(
          p1 = f
     )(
          p2 = g
     )|
\qed
\end{eqnarray*}

Os diagramas podem ser produzidos recorrendo à \emph{package} \LaTeX\
\href{https://ctan.org/pkg/xymatrix}{xymatrix}, por exemplo:
\begin{eqnarray*}
\xymatrix@@C=2cm{
    |Nat0|
           \ar[d]_-{|cataNat g|}
&
    |1 + Nat0|
           \ar[d]^{|id + (cataNat g)|}
           \ar[l]_-{|inNat|}
\\
     |B|
&
     |1 + B|
           \ar[l]^-{|g|}
}
\end{eqnarray*}

\section{Regra prática para a recursividade mútua em |Nat0|}\label{sec:mr}

Nesta disciplina estudou-se como fazer \pd{programação dinâmica} por cálculo,
recorrendo à lei de recursividade mútua.\footnote{Lei (\ref{eq:fokkinga})
em \cite{Ol18}, página \pageref{eq:fokkinga}.}

Para o caso de funções sobre os números naturais (|Nat0|, com functor
|fF X = 1 + X|) é fácil derivar-se da lei que foi estudada uma
  \emph{regra de algibeira}
  \label{pg:regra}
que se pode ensinar a programadores que não tenham estudado
\cp{Cálculo de Programas}. Apresenta-se de seguida essa regra, tomando como
exemplo o cálculo do ciclo-\textsf{for} que implementa a função de Fibonacci,
recordar o sistema:
\begin{spec}
fib 0 = 1
fib(n+1) = f n

f 0 = 1
f (n+1) = fib n + f n
\end{spec}
Obter-se-á de imediato
\begin{code}
fib' = p1 . for loop init where
   loop(fib,f)=(f,fib+f)
   init = (1,1)
\end{code}
usando as regras seguintes:
\begin{itemize}
\item O corpo do ciclo |loop| terá tantos argumentos quanto o número de funções
mutuamente recursivas.
\item Para as variáveis escolhem-se os próprios nomes das funções, pela ordem
que se achar conveniente.\footnote{Podem obviamente usar-se outros símbolos,
mas numa primeira leitura dá jeito usarem-se tais nomes.}
\item Para os resultados vão-se buscar as expressões respectivas, retirando
a variável |n|.
\item Em |init| coleccionam-se os resultados dos casos de base das funções,
pela mesma ordem.
\end{itemize}
Mais um exemplo, envolvendo polinómios do segundo grau $ax^2 + b x + c$ em |Nat0|.
Seguindo o método estudado nas aulas\footnote{Secção 3.17 de \cite{Ol18} e tópico
\href{https://www4.di.uminho.pt/~jno/media/cp/}{Recursividade mútua}
nos vídeos de apoio às aulas teóricas.},
de $f\ x = a x^2 + b x + c$ derivam-se duas funções mutuamente recursivas:
\begin{spec}
f 0 = c
f (n+1) = f n + k n

k 0 = a + b
k(n+1) = k n + 2 a
\end{spec}
Seguindo a regra acima, calcula-se de imediato a seguinte implementação, em Haskell:
\begin{code}
f' a b c = p1 . for loop init where
  loop(f,k) = (f+k,k+2*a)
  init = (c,a+b) 
\end{code}

\section{O mónade das distribuições probabilísticas} \label{sec:probabilities}
%format B = "\mathit B"
%format C = "\mathit C"
Mónades são functores com propriedades adicionais que nos permitem obter
efeitos especiais em progra\-mação. Por exemplo, a biblioteca \Probability\
oferece um mónade para abordar problemas de probabilidades. Nesta biblioteca,
o conceito de distribuição estatística é captado pelo tipo
\begin{eqnarray}
     |newtype Dist a = D {unD :: [(a, ProbRep)]}|
     \label{eq:Dist}
\end{eqnarray}
em que |ProbRep| é um real de |0| a |1|, equivalente a uma escala de $0$ a
$100 \%$.

Cada par |(a,p)| numa distribuição |d::Dist a| indica que a probabilidade
de |a| é |p|, devendo ser garantida a propriedade de  que todas as probabilidades
de |d| somam $100\%$.
Por exemplo, a seguinte distribuição de classificações por escalões de $A$ a $E$,
\[
\begin{array}{ll}
A & \rule{2mm}{3pt}\ 2\%\\
B & \rule{12mm}{3pt}\ 12\%\\
C & \rule{29mm}{3pt}\ 29\%\\
D & \rule{35mm}{3pt}\ 35\%\\
E & \rule{22mm}{3pt}\ 22\%\\
\end{array}
\]
será representada pela distribuição
\begin{code}
d1 :: Dist Char
d1 = D [('A',0.02),('B',0.12),('C',0.29),('D',0.35),('E',0.22)]
\end{code}
que o \GHCi\ mostrará assim:
\begin{Verbatim}[fontsize=\small]
'D'  35.0%
'C'  29.0%
'E'  22.0%
'B'  12.0%
'A'   2.0%
\end{Verbatim}
É possível definir geradores de distribuições, por exemplo distribuições \emph{uniformes},
\begin{code}
d2 = uniform (words "Uma frase de cinco palavras")
\end{code}
isto é
\begin{Verbatim}[fontsize=\small]
     "Uma"  20.0%
   "cinco"  20.0%
      "de"  20.0%
   "frase"  20.0%
"palavras"  20.0%
\end{Verbatim}
distribuição \emph{normais}, eg.\
\begin{code}
d3 = normal [10..20]
\end{code}
etc.\footnote{Para mais detalhes ver o código fonte de \Probability, que é uma adaptação da
biblioteca \PFP\ (``Probabilistic Functional Programming''). Para quem quiser saber mais
recomenda-se a leitura do artigo \cite{EK06}.}
|Dist| forma um \textbf{mónade} cuja unidade é |return a = D [(a,1)]| e cuja composição de Kleisli
é (simplificando a notação)
\begin{spec}
  ((kcomp f g)) a = [(y,q*p) | (x,p) <- g a, (y,q) <- f x]
\end{spec}
em que |g: A -> Dist B| e |f: B -> Dist C| são funções \textbf{monádicas} que representam
\emph{computações probabilísticas}.

Este mónade é adequado à resolução de problemas de \emph{probabilidades e estatística} usando programação funcional, de forma elegante e como caso particular da programação monádica.

\section{Código fornecido}\label{sec:codigo}

\subsection*{Problema 1}
Alguns testes para se validar a solução encontrada:
\begin{code}
test a b c = map (fbl a b c) x == map (f a b c) x where x = [1..20]  

test1 = test 1 2 3
test2 = test (-2) 1 5
\end{code}

\subsection*{Problema 2}

\textbf{Verificação}: a árvore de tipo \Exp\ gerada por
\begin{code}
acm_tree = tax acm_ccs
\end{code}
deverá verificar as propriedades seguintes:
\begin{itemize}
\item |expDepth acm_tree == 7| (profundidade da árvore);
\item |length (expOps acm_tree) == 432| (número de nós da árvore);
\item |length (expLeaves acm_tree) == 1682| (número de folhas da árvore).\footnote{Quer dizer, o número total de nodos e folhas é |2114|, o número de linhas do texto dado.}
\end{itemize}
O resultado final
\begin{code}
acm_xls  = post acm_tree
\end{code}
não deverá ter tamanho inferior ao total de nodos e folhas da árvore.

\subsection*{Problema 3}
Função para visualização em \svg:
\begin{code}
drawSq x = picd'' [ Svg.scale 0.44 (0,0) (x >>= sq2svg) ]
sq2svg (p,l) = (color "#67AB9F" . polyg) [ p, p .+ (0,l), p .+ (l,l), p .+ (l,0) ]
\end{code}
Para efeitos de temporização:
\begin{code}
await = threadDelay 1000000
\end{code}

\subsection*{Problema 4}
Rankings:
\begin{code}
rankings = [
         ("Argentina",4.8),
         ("Australia",4.0),
         ("Belgium",5.0),
         ("Brazil",5.0),
         ("Cameroon",4.0),
         ("Canada",4.0),
         ("Costa Rica",4.1),
         ("Croatia",4.4),
         ("Denmark",4.5),
         ("Ecuador",4.0),
         ("England",4.7),
         ("France",4.8),
         ("Germany",4.5),
         ("Ghana",3.8),
         ("Iran",4.2),
         ("Japan",4.2),
         ("Korea Republic",4.2),
         ("Mexico",4.5),
         ("Morocco",4.2),
         ("Netherlands",4.6),
         ("Poland",4.2),
         ("Portugal",4.6),
         ("Qatar",3.9),
         ("Saudi Arabia",3.9),
         ("Senegal",4.3),
         ("Serbia",4.2),
         ("Spain",4.7),
         ("Switzerland",4.4),
         ("Tunisia",4.1),
         ("USA",4.4),
         ("Uruguay",4.5),
         ("Wales",4.3)]
\end{code}
Geração dos jogos da fase de grupos:
\begin{code}
generateMatches = pairup
\end{code}
Preparação da árvore do ``mata-mata'':
\begin{code}
arrangement = (>>= swapTeams) . chunksOf 4 where
  swapTeams [[a1,a2],[b1,b2],[c1,c2],[d1,d2]] = [a1,b2,c1,d2,b1,a2,d1,c2]
\end{code}
Função proposta para se obter o \emph{ranking} de cada equipa:
\begin{code}
rank x = 4 ** (pap rankings x - 3.8)
\end{code}
Critério para a simulação não probabilística dos jogos da fase de grupos:
\begin{code}
gsCriteria = s . split (id >< id) (rank >< rank) where
  s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
                           if d > 0.5 then Just s1
                                      else if d < -0.5 then Just s2
                                                       else Nothing
\end{code}
Critério para a simulação não probabilística dos jogos do mata-mata:
\begin{code}
koCriteria = s . split (id >< id) (rank >< rank) where
  s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
                           if d == 0 then s1
                                     else if d > 0 then s1 else s2
\end{code}
Critério para a simulação probabilística dos jogos da fase de grupos:
\begin{code}
pgsCriteria = s . split (id >< id) (rank >< rank) where
  s ((s1, s2), (r1, r2)) =
     if abs(r1-r2) > 0.5 then fmap Just (pkoCriteria(s1,s2)) else f (s1,s2)
  f = D . ((Nothing,0.5):) . map (Just><(/2)) . unD . pkoCriteria
\end{code}
Critério para a simulação probabilística dos jogos do mata-mata:
\begin{code}
pkoCriteria (e1, e2) = D [(e1, 1 - r2 / (r1 + r2)), (e2, 1 - r1 / (r1 + r2))] where
    r1 = rank e1
    r2 = rank e2
\end{code}
Versão probabilística da simulação da fase de grupos:\footnote{Faz-se ``trimming'' das distribuições para reduzir o tempo de simulação.}
\begin{code}
psimulateGroupStage = trim .  map (pgroupWinners pgsCriteria)
trim = top 5 . sequence . map (filterP.norm)  where
   filterP (D x) = D [(a,p) | (a,p) <- x, p > 0.0001 ]
   top n = vec2Dist . take n . reverse . presort snd . unD 
   vec2Dist x = D [ (a,n/t) | (a,n) <- x ] where t = sum(map snd x) 
\end{code}
Versão mais eficiente da |pwinner| dada no texto principal, para diminuir o tempo de cada
simulação:
\begin{code}
pwinner :: Dist Team
pwinner = mbin f x >>= pknockoutStage where
   f(x,y) = initKnockoutStage(x++y)
   x = split (g . take 4) (g . drop 4) groups
   g = psimulateGroupStage . genGroupStageMatches
\end{code}
Auxiliares:
\begin{code}
best n = map fst . take n . reverse . presort p2

consolidate :: (Num d, Eq d, Eq b) => [(b, d)] -> [(b, d)]
consolidate = map (id><sum) . collect

collect :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
collect x = nub [ k |-> [ d' | (k',d') <- x , k'==k ] | (k,d) <- x ]
\end{code}
Função binária monádica |f|:
\begin{code}
mmbin :: Monad m => ((a, b) -> m c) -> (m a, m b) -> m c
mmbin f (a,b) = do { x <- a ; y <- b ; f(x,y) }
\end{code}
Monadificação de uma função binária |f|:
\begin{code}
mbin :: Monad m => ((a, b) -> c) -> (m a, m b) -> m c
mbin = mmbin . (return.)
\end{code}
Outras funções que podem ser úteis:
\begin{code}
(f `is` v) x = (f x) == v

rcons(x,a) = x++[a]
\end{code}

%----------------- Soluções dos alunos -----------------------------------------%

\section{Soluções dos alunos}\label{sec:resolucao}
Os alunos devem colocar neste anexo as suas soluções para os exercícios
propostos, de acordo com o ``layout'' que se fornece. Não podem ser
alterados os nomes ou tipos das funções dadas, mas pode ser adicionado
texto, diagramas e/ou outras funções auxiliares que sejam necessárias.

Valoriza-se a escrita de \emph{pouco} código que corresponda a soluções
simples e elegantes.

\subsection*{Problema 1}

\begin{spec}
f a b c 0 = 0
f a b c 1 = 1
f a b c 2 = 1
f a b c (n+3) = a * f a b c (n+2) + b * f a b c (n+1) + c * f a b c n
\end{spec}

De modo a resolver o primeiro problema, definimos duas funções auxiliares:

\begin{spec}
g a b c n = f a b c (n+1)

j a b c n = f a b c (n+2)
\end{spec}

E podemos relacionar a função |g| com a função |j|:

\begin{spec}
j a b c n = g a b c (n+1) = f a b c (n+2)
\end{spec}

Logo, possuímos a seguinte recursividade mútua:

\begin{eqnarray*}
\begin{cases}
      \begin{cases}
        |j a b c 0 = 1|\\
        |j a b c (n+1) = a * j a b c n + b * g a b c n + c * f a b c n|
      \end{cases}\\
      \begin{cases}
        |g a b c 0 = 1|\\  
        |g a b c (n+1) = j a b c n| 
      \end{cases}\\
      \begin{cases}
        |f a b c 0 = 0|\\
        |f a b c (n+1) = g a b c n|
      \end{cases}
\end{cases}
\end{eqnarray*}

Através da lei da recursividade mútua, para os naturais, sabemos que: 

\begin{eqnarray*}
\start  \begin{cases}
            \begin{cases}
              |(j a b c) . (const 0) = (const 1)|\\
              |(j a b c) . succ = add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)  . split (split (j a b c) (g a b c)) (f a b c)|
            \end{cases}\\
            \begin{cases}
              |(g a b c) . (const 0) = (const 1)|\\
              |(g a b c) . succ = p1 . p1 . split (split (j a b c) (g a b c)) (f a b c)|
            \end{cases}\\
            \begin{cases}
              |(f a b c) . (const 0) = (const 0)|\\
              |(f a b c) . succ = p2 . p1 . split (split (j a b c) (g a b c)) (f a b c)|
            \end{cases}
        \end{cases}
%
\just\equiv{ 3x Eq-+; 3x Fusao-+ }
%
\begin{cases}
  |(j a b c) . either (const 0) succ = either (const 1)  (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)  . split (split (j a b c) (g a b c)) (f a b c))|\\
  |(g a b c) . either (const 0) succ = either (const 1) (p1 . p1 . split (split (j a b c) (g a b c)) (f a b c))|\\
  |(f a b c) . either (const 0) succ = either (const 0) (p2 . p1 . split (split (j a b c) (g a b c)) (f a b c))|
\end{cases}
%
\just\equiv{ Defenição de in; Natural-id }
%
\begin{cases}
  |(j a b c) . in = either ((const 1) . id)  (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)  . split (split (j a b c) (g a b c)) (f a b c))|\\
  |(g a b c) . in = either ((const 1) . id) (p1 . p1 . split (split (j a b c) (g a b c)) (f a b c))|\\
  |(f a b c) . in = either ((const 0) . id) (p2 . p1 . split (split (j a b c) (g a b c)) (f a b c))|       
\end{cases}
%
\just\equiv{ 3x Absorção-+ }
%
\begin{cases}
  |(j a b c) . in = (either (const 1)  (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2))) . 1 + split (split (j a b c) (g a b c)) (f a b c)|\\
  |(g a b c) . in = (either (const 1) (p1 . p1)) . 1 + split (split (j a b c) (g a b c)) (f a b c)|\\
  |(f a b c) . in = (either (const 0) (p2 . p1)) . 1 + split (split (j a b c) (g a b c)) (f a b c)|
\end{cases}
%
\just\equiv{ Fokkinga }
%
|split (split (j a b c) (g a b c)) (f a b c) = cata (split(split (either (const 1)  (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2))) (either (const 1) (p1 . p1))) (either (const 0) (p2 . p1)))|
\qed
\end{eqnarray*}

Mostrando assim que a podemos escrever a função |f| a partir de um catamorfismo. Como obtemos o catamorfismo, podemos 
converter para um ciclo for de acordo com a definição:

\begin{spec}
for b i = cata (either (const i) b)
\end{spec}

Assim sendo:

\begin{eqnarray*}
\start
|cata (split (split (either (const 1)  (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2))) (either (const 1) (p1 . p1))) (either (const 0) (p2 . p1)))|
%
\just\equiv{ Lei da troca }
%
|cata (split (either (split (const 1) (const 1)) (split (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)) (p1 . p1))) (either (const 0) (p2 . p1)))|
%
\just\equiv{ Lei da troca }
%
|cata (either (split (split (const 1) (const 1)) (const 0)) (split (split (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)) (p1 . p1)) (p2 . p1)))|
\qed
\end{eqnarray*}

Para sabermos o valor de inicialização do ciclo, igualamos a seguinte expressão:

\begin{eqnarray*}
\start
| const i = split (split (const 1) (const 1)) (const 0)|
%
\just\equiv{ Universal-x }
% 
|lcbr(
  p1 . (const i) = split (const 1) (const 1)
)(
  p2 . (const i) = (const 0)
)|
%
\just\equiv{ Universal-x; Fusão-const }
%
|lcbr(
  lcbr(
    p1 . p1 . (const i) = (const 1)
  )(
    p2 . p1 . (const i) = (const 1)
  )
)(
  const (p2 . i) = const 0
)|
%
\just\equiv{ 2x Fusão-const; |(const a) = (const b)| se |a=b|}
%
|lcbr(
  lcbr(
    const (p1 . p1 . i) = (const 1)
  )(
    const (p2 . p1 . i) = (const 1)
  )
)(
  p2 . i = 0
)|
%
\just\equiv{ 2x |(const a) = (const b)| se |a=b|}
%
|lcbr(
  lcbr(
    p1 . p1 . i = 1
  )(
    p2 . p1 . i = 1
  )
)(
  p2 . i = 0
)|
%
\just\equiv{ Universal-x }
%
|lcbr(
  p1 . i = (1,1)
)(
  p2 . i = 0
)|
%
\just\equiv{ Universal-x }
%
|i = ((1,1),0)|
\qed
\end{eqnarray*}

Obtendo o |initial|.

Para apresentarmos a função do corpo do ciclo de forma mais amigável à preceção do utilizar vamos passá-la para a notação \emph{point wise}, introduzindo variàveis: 

\begin{eqnarray*}
\start
|(split (split (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)) (p1 . p1)) (p2 . p1)) ((h,k),l)|
%
\just\equiv{ Def-split }
% 
|(split (add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2)) (p1 . p1) ((h,k),l), (p2 . p1) ((h,k),l) )|
%
\just\equiv{ Def-split; Def-comp }
% 
|((add . split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2) ((h,k),l), (p1 . p1) ((h,k),l) ), p2 ( p1 ((h,k),l) ) ) |
%
\just\equiv{ 2x Def-comp; Def-proj }
% 
|((add ((split (add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1)) ((c*) . p2) ((h,k),l) )), p1 ( p1 ((h,k),l) ) ), p2 (h,k) ) |
%
\just\equiv{ Def-split; 2x Def-proj }
% 
|((add ( add . split ((a*) . p1 . p1 ) ((b*) . p2 . p1) ((h,k),l), ((c*) . p2) ((h,k),l) ), p1 (h,k) ), k ) |
%
\just\equiv{ 2x Def-comp; Def-proj }
% 
|((add ( add ( ((a*) . p1 . p1 ) ((h,k),l), ((b*) . p2 . p1) ((h,k),l)), (c*) (p2 (h,l)) ), h ), k ) |
%
\just\equiv{ 2x Def-comp; Def-proj }
% 
|((add ( add ( ((a*) . p1) ( p1 ((h,k),l)), ((b*) . p2) ( p1 ((h,k),l))), (c*) l ), h ), k ) |
%
\just\equiv{ 2x Def-proj; Definição de |*| }
% 
|((add ( add ( ((a*) . p1) (h,k), ((b*) . p2) (h,k)), c * l ), h ), k ) |
%
\just\equiv{ 2x Def-comp }
% 
|((add ( add ( (a*) ( p1 (h,k)), (b*) ( p2 (h,k))), c * l ), h ), k ) |
%
\just\equiv{ 2x Def-proj }
% 
|((add ( add ( (a*) h, (b*) k), c * l ), h ), k ) |
%
\just\equiv{ 2x Definição de |*| }
% 
|((add ( add ( a * h, b * k), c * l ), h ), k ) |
%
\just\equiv{ Definição de |add| }
% 
|((add ( a * h + b * k, c * l ), h ), k ) |
\just\equiv{ Definição de |add| }
% 
|((a * h + b * k + c * l , h ), k ) |
\qed
\end{eqnarray*}

Funções auxiliares pedidas:
\begin{code}
loop a b c ((j,g),f) = ((a*j+b*g+c*f,j),g)
initial = ((1,1),0)
wrap = p2
\end{code}

\subsection*{Problema 2}

De forma a resolvermos o problema 2 é necessário recorrer ao seguinte diagrama de um anamorfismo:

\begin{eqnarray*}
\xymatrix{
  |Exp S S| & & S + S \times (|Exp S S|)^*\ar[ll]_{|inExp|} \\
  S^*\ar@@/_1.5pc/[rr]_{|gene|}\ar[r]^(0.35){|out|}\ar[u]^{|tax|} & S + S \times S^*\ar[r]^(0.45){\cdots} & S + S \times (S^*)^*\ar[u]_{id + id \times tax^*}
}
\end{eqnarray*}

O gene será uma a composição de duas funções, a |out| e um função que é um |either|.
Para podermos definir as duas componentes do |either| é necessário perceber o resultado da função |out|.
A função |out| é o |out| das listas não vazias. No caso da lista só com um elemento,
esse é etiquetado à esquerda e neste caso aplicamos a função identidade, visto que, este elemento será uma variável na árvore de expressões.
No caso da lista com mais de um elemento divide a cabeça e a causa e o par é etiquetado à direita.
Para facilidade da interpertar o raciocínio apresentamos, através de um caso específico, como fica a saída:

\begin{spec}
("CCS",["    General and reference",
        "        Document types",
        "            Surveys and overviews",
        "            Reference works",
        "            General conference proceedings",
        "            Biographies",
        "            General literature",
        "            Computing standards, RFCs and guidelines",
        "        Cross-computing tools and techniques"])
\end{spec}

Neste segundo caso na cabeça ficará o primeiro elemento da lista, ou seja, a |String| não identada que é um operador, à qual basta aplicar a função identidade.
Na cauda da lista ficam as |String| com identação. Para remover a identação é necessário remover os 4 espaços 
de todos os elementos e para isso usamos a função |map (take 4)| ficando o par da seguinte forma:

\begin{spec}
("CCS",["General and reference",
        "    Document types",
        "        Surveys and overviews",
        "        Reference works",
        "        General conference proceedings",
        "        Biographies",
        "        General literature",
        "        Computing standards, RFCs and guidelines",
        "    Cross-computing tools and techniques"])
\end{spec}

Em seguida é necessário dividir esta lista em sublistas, ou seja, fazer um agrupamento.
Cada lista nova terá a cabeça, ou seja, a |String| sem identação e as |String| com hierarquia inferior a esta, com identação.  
Para isso usamos a seguinte função:

  groupBy ($\backslash$x y = take 4 y == ``\hspace{1cm}'')


Esta condição permite que sejam geradas as lista da forma pretendida, visto que, a função groupBy mantem uma lista atual e faz a comparação do último elemento da lista atual com o próximo elemento da lista,
se é verificada adiciona o elemento à lista atual, caso contrário insere a lista atual na lista de sublistas e adiciona uma nova lista atual com este elemento.
Logo, no nosso caso em especifico:

\begin{spec}
("CCS",[["General and reference",
        "    Document types",
        "        Surveys and overviews",
        "        Reference works",
        "        General conference proceedings",
        "        Biographies",
        "        General literature",
        "        Computing standards, RFCs and guidelines",
        "    Cross-computing tools and techniques"]])
\end{spec}

Assim sendo, o gene de |tax|:
\begin{code}
gene = (id -|- (id >< groupBy (\x y -> take 4 y == "    ") . map (drop 4))) . out
\end{code}

A função de pós-processamento pode ser definida através de um catamorfismo com o seguinte diagrama:

\begin{eqnarray*}
\xymatrix@@C=3cm{
    |Exp S S|
           \ar[d]_-{|cataExp g|}
&
    S + S \times (|Exp S S|)^*
           \ar[d]^{|id + id| \times |(cataExp g)|}
           \ar[l]_-{|inExp|}
\\
     (S^*)^*
&
     S + S \times ((S^*)^*)^*
           \ar[l]^-{|gnp|}
}
\end{eqnarray*}

Depois de identificarmos o catamorfismo de árvores de expressão, passamos à fase de identificar qual o seu gene.  
No caso do lado esquerdo apenas precisamos de colocar a |String| dentro de uma lista e esta dentro de outra lista. 
No caso do lado direito é necessário perceber qual é o resultado do lado direito do par, que são as listas com os caminhos que é possível tomar a partir da |String| do lado esquerdo do par.   
Assim sendo, necessitamos de introduzir à cabeça de cada um desses caminhos a |String| do lado esquerdo. 
Para isso, primeiro a lista com as listas dos caminhos são concatenadas para possuírmos a lista com os caminhos.   
Em seguida, acrescentamos à cabeça de todos os caminhos a |String| do lado esquerdo e acrescentamos mais um caminho para que seja possível chegar ao nível desta |String|, visto que queremos todos os caminhos da árvore.   

Culminando na função de pós-processamento:
\begin{code}
post = cataExp gnp
  where gnp = either gnp1 gnp2
        gnp1 x = [[x]]
        gnp2 (x,l) = [x] : map (x:) (concat l)

\end{code}

\subsection*{Problema 3}
A primeira parte do problema 3 consiste no cálculo dos gene das funções rose2List e squares. 

Estes dois genes quando compostos formam o seguinte hilomorfismo:

\begin{spec} 
sierpinski :: (Square, Int) -> [Square]
sierpinski = hyloRose gr2l  gsq
\end{spec}

Para resolvermos o hilomorfismo abordado anteriormente, nós começamos por descobrir o gene do catamorfismo rose2List.Sendo assim obtivemos o seguinte diagrama:
\begin{eqnarray*}
\xymatrix@@C=3cm{
    |Rose A|
           \ar[d]_-{|cataRose gr2l|}
&
    A \times (|Rose A|)^*
           \ar[d]^{|id| \times |cataRose gr2l|}
           \ar[l]_-{|inRose|}
\\
     A^*
&
     A \times (A^*)^*
           \ar[l]^-{|gr2l|}
}
\end{eqnarray*}
Tendo em conta o diagrama apresentado podemos reparar que o único ponto de falha existente na função é no cálculo do seu gene uma vez que tudo o resto já se encontra 
devidamente definido. 
Tendo em atenção aos tipos de entrada e de saída que o gene possui, conseguimos perceber claramente qual o trabalho desenvolvido por este. 

Se repararmos o gene recebe um par constituído pela cabeça de uma lista, de tipo A, e uma lista de listas com elementos do tipo A. 
O gene devolve apenas uma lista,com elementos do tipo A.

Sendo assim conseguimos perceber que o gene irá ter de concatenar a lista de listas, formando apenas uma lista e colocando o elemento A á cabeça da lista criada.

Com base nesta explicação do diagrama conseguimos perceber que a função rose2List pode ser definida da seguinte forma:
\begin{code}
rose2List = cataRose gr2l 

gr2l (a,l)=a:concat l
\end{code}

Tal como aconteceu para o cálculo do gene do catamorfismo, adotamos a mesma estratégia para o cálculo do gene do anamorfismo. Sendo assim o diagrama que corresponde
ao anamorfismo é o seguinte:
\begin{eqnarray*}
\xymatrix{
  |Rose Square| & & Square \times (|Rose Square|)^*\ar[ll]_{|inRose|} \\
  Square \times N_0\ar@@/_1.5pc/[rr]_{|gsq|}\ar[r]^(0.55){|outNat.p2|}\ar[u]^{|anaRose gsq|} & 1+(N_0)\ar[r]^(0.35){\cdots} & Square \times (Square,N_0)^*\ar[u]_{|id| \times |map(cataRose gr2l)|}
}
\end{eqnarray*}
Visualizando o diagrama do anamorfismo, conseguimos perceber que apenas temos de calcular o gene do anamorfismo. 
Sendo assim, se repararmos no diagrama do anamorfismo, o gene recebe um par composto por (Square,N0) e tem de devolver um par composto por 
uma square e uma lista composta por (Square,N_0) . 

Com base no que foi dito anteriormente, conseguimos perceber que o gene tem de devolver uma square caso o natural seja igual a 0, ou então, devolve uma lista
de pares (Square,N0) caso o N0 seja diferente de 0. 

Esta lista, "contém" os quadrados do tapete de Sierpinski. Assim cada elemento da lista, tem diferentes coordenadas. 

Sendo assim, conseguimos perceber qual o trabalho que o gene (gsq) irá ter de realizar.
\begin{code}

squares = anaRose gsq

gsq = split gsq1 gsq2
gsq1 (((x,y),z),w) =((x+z/3,y+z/3),z/3)
gsq2 (((x,y),z),w) = if w>0 then [(((x,y),z/3),w-1),  
                                  (((x+z/3,y),z/3),w-1),  
                                  (((x+(2*z/3),y),z/3),w-1), 
                                  (((x,y+z/3),z/3),w-1),
                                  (((x+2*z/3,y+z/3),z/3),w-1),
                                  (((x,y+2*z/3),z/3),w-1), 
                                  (((x+z/3,y+2*z/3),z/3),w-1),
                                  (((x+2*z/3,y+2*z/3),z/3),w-1)] 
                    else []
\end{code}
Nesta fase do problema 3, surge uma nova abordagem ao problema. Para explicar esta nova abordagem temos o seguinte:

- Na primeira abordagem feita ao problema, nós temos de inserir a square inicial e a profundidade até á qual queremos simular;

- Nesta nova abordagem, nós apenas teremos de inserir a profundidade até á qual queremos simular e a função dar-nos-á os tapetes devidamente prontos;

Desta forma o resultado final, pode ser representado por este hilomorfismo:

% \begin{spec}
% constructSierp ::Int -> IO [()]

% constructSierp = present · carpets
% \end{spec}

Com base na útlima abordagem conseguimos perceber, que iremos ter duas funções: a carpets e a present.
A função carpets recebe como input um inteiro, que representa a profundidade N e devolve uma lista de lista de Squares, que representam os tapetes de profundidade 0 
até á profundidade N-1.
Sendo assim, para uma profundidade igual a 0, não é criado qualquer tipo de tapete. Logo a lista resultado é uma lista vazia.

Para profundidades superiores a 0, é feito é o seguinte:

1º- Cálculo da carpets para profundidades inferiores á dada como parâmetro;

2º- Cálculo do sierpinski para profundidade inferior á dada como parâmetro.

Com isto percebemos como funciona a função carpets.
\begin{code}
carpets 0 = []
carpets n =carpets (n-1) ++ [sierpinski(((0,0),32),n-1)]
\end{code}
A função present consome o resultado produzido pela carpets uma vez que ambas compõem um hilomorfismo. 

Assim o trabalho realizado pela função present é o seguinte:

-Cada elemento da lista [[Square]], que representa um tipo de tapete de sierpinski, é desenhado no ecrã(I\O) pela função drawSq;

-Entre o desenho de cada tapete no ecrã,o programa tem de "parar" durante um determinado intervalo de tempo, sendo assim é chamada a função await.

O mmap é utilizado para aplicar o monade de I\O a cada elemento da lista dada como input. 
Desta forma, conseguimos perceber o funcionamento da função present que é composta por um monade de I\O. 

\begin{code}
present = mmap (\ l -> do {drawSq l;await})
\end{code}
Depois de calculadas estas duas funções, conseguimos por fim finalizar o hilomorfismo inicial, que constroi tapetes de Sierpinski precisando apenas
de uma determinada profundidade.
\subsection*{Problema 4}

Neste problema 4, é nos pedido que façamos uma espécie de simulação do campeonato do mundo que ocorreu este ano. Para isso vamos ter duas partes:

-Uma versão não probabilística em que se sabe à partida para um dado jogo, quem o vai vencer.

-Uma versão com probabilidades, onde se usa o mónade Dist(que usa distribuições probabilísticas).

\subsubsection*{Versão não probabilística}

Nesta primeira fase não probabilística, primeiramente, é nos pedido para definir uma alternativa à função genérica consolidate, que seja um catamorfismo de listas.
Para isso, tivemos de definir o gene do catamorfismo que está indicado em baixo.

Gene de |consolidate'|:

\begin{code}
cgene :: (Eq a, Num b) => Either () ((a, b), [(a, b)])-> [(a, b)]
cgene = either cgene1 cgene2
  where cgene1 = nil
        cgene2 ((a,b), []) = [(a,b)]
        cgene2 ((a,b), (a1,b1) : l) = if a == a1 then (a,b+b1) : l
                         else (a1,b1) : cgene2 ((a,b), l)
\end{code}

Geração dos jogos da fase de grupos:

Para decidir quem é o vencedor de um determinado jogo, tivemos de definir a função matchResult, que recebe como
argumentos a função gsCriteria que mediante um certo critério, calcula o resultado de um jogo, e um argumento de tipo Match, que é um par de Teams.

A função encontra-se definida em baixo.

\begin{code}
matchResult gsCriteria (t1,t2) = if gsCriteria (t1,t2) == Just t1 then [(t1,3),(t2,0)]
                                 else if gsCriteria (t1,t2) == Just t2 then [(t1,0),(t2,3)]
                                 else [(t1,1),(t2,1)]
\end{code}

De seguida, foi nos pedido para definir a função pairup, em que generateMatches se baseia.
Esta função, basicamente recebe uma lista de elementos de um certo tipo b, e cria uma lista de pares de elementos desse tipo.

\begin{code}
pairup [] = []
pairup (x:xs) = map(\y -> (x,y)) xs ++ pairup xs 
\end{code}

Por último, foi nos pedido para definir o gene glt, que será usado na função initKnockoutStage por um anamorfismo.
O gene encontra-se definido em baixo.

\begin{code}
glt :: Eq a => [a] -> Either a ([a], [a])
glt [x] = i1 x
glt l = i2 (splitAt n l)
       where n = div (length l) 2
\end{code}

O diagrama representativo do anamorfismo utilizado na função initKnockoutStage é o seguinte:

\begin{eqnarray*}
\xymatrix{
    |LTree A| & & A -|- (|LTree A| \times |LTree A|) ^* \ar[ll]_{|inLTree|} \\
    (A^*)\ar@@/_1.5pc/[rr]_{|glt|}\ar[u]^{|anaList gsq|} & A -|- (A^* \times A)^*\ar[u]_{|id + anaList glt + anaList glt|}
}
\end{eqnarray*}

\subsubsection*{Versão probabilística}
\begin{code}
pinitKnockoutStage l =  do {return (initKnockoutStage l)}

pgroupWinners :: (Match -> Dist (Maybe Team)) -> [Match] -> Dist [Team]
pgroupWinners criteria = fmap (best 2 . consolidate' . concat) . mmap (pmatchResult criteria)


pmatchResult criteria m = do { r <- criteria m; return (f m r)}
                          where f (t1,t2) Nothing = [(t1,1),(t2,1)]
                                f (t1,t2) (Just t) = if t == t1 then [(t1,3),(t2,0)] else [(t1,0),(t2,3)]
\end{code}

%----------------- Índice remissivo (exige makeindex) -------------------------%

\printindex

%----------------- Bibliografia (exige bibtex) --------------------------------%

\bibliographystyle{plain}
\bibliography{cp2223t}

%----------------- Fim do documento -------------------------------------------%

\end{document}
