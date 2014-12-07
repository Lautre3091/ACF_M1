theory tp89

imports Main "~~/src/HOL/Library/Efficient_Nat"

begin
(* c * m * id *)
type_synonym transid= "nat*nat*nat"

datatype message= 
  Pay transid nat  
| Ack transid nat
| Cancel transid

type_synonym transaction= "transid * nat"

datatype etat = Validee | EnCours | Annulee | PrixProposer |PrixDemander

type_synonym trans= "etat * transid * nat * nat"
type_synonym transBdd = "trans list"

fun equal:: "transid \<Rightarrow> transid \<Rightarrow> bool"
where
  " equal (c1,m1,idT1) (c2,m2,idT2) = (c1=c2 & m1=m2 & idT1=idT2)"

fun annuleOffre:: "transid \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
  "annuleOffre tId [] = [(Annulee, tId, 0, 0)]"
| "annuleOffre tId1 ((etat, tId2, offre, demande)#s) = (
    if (equal tId1 tId2) then (Annulee, tId2,offre, demande)#s
    else (etat, tId2, offre, demande)#(annuleOffre tId1 s))"

fun offreClient::"transid \<Rightarrow> nat \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
"offreClient tId x [] = (if (x > 0) then (PrixProposer, tId, x, 0)#[] else [])" |
"offreClient tId1 x ((etat, tId2, pClient, pMarchand)#s) = 
                  (if (equal tId1 tId2)
                    then (case etat of
                    Validee \<Rightarrow> (etat, tId2, pClient, pMarchand)#s |
                    EnCours \<Rightarrow> (if x > pClient
                                then 
                                  (if x \<ge> pMarchand (*prendre en compte la nouvelle offre ...*)
                                    then (Validee, tId2, x, pMarchand)#s  (*...et la valider*)
                                    else (etat, tId2, x, pMarchand)#s) (*...sans la valider*)
                                else (etat, tId2, pClient, pMarchand)#s (*ignorer l'offre*)
                                )|
                    Annulee \<Rightarrow> (etat, tId2,pClient , pMarchand)#s |
                    PrixProposer \<Rightarrow> (if (x > pClient)
                              then (etat, tId2, x, pMarchand)#s (*on edite le montant avec le nouveau (plus grand) *)
                              else (etat, tId2, pClient, pMarchand)#s (*on ne touche pas a l'offre*)
                            ) |
                    PrixDemander \<Rightarrow> (if (x > 0)
                                then
                                    (if(x \<ge> pMarchand) (*prendre en compte la nouvelle offre ...*)
                                    then (Validee, tId2, x, pMarchand)#s  (*...et la valider*)
                                    else (EnCours, tId2, x, pMarchand)#s) (*...sans la valider*)
                                else (etat, tId2, pClient, pMarchand)#s (*ignorer l'offre*)
                              )
                        )
                    else((etat, tId2, pClient, pMarchand)#(offreClient tId1 x s))
                  )"

fun offreMarchand::"transid \<Rightarrow> nat \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
"offreMarchand tId x [] = (if (x > 0) then (PrixDemander, tId, 0, x)#[] else [])" |
"offreMarchand tId1 x ((etat, tId2, pClient, pMarchand)#s) = 
                  (if (equal tId1 tId2)
                    then (case etat of
                      Validee \<Rightarrow> (etat, tId2, pClient, pMarchand)#s |
                      EnCours \<Rightarrow> (if x < pMarchand
                                then 
                                  (if(x \<le> pClient) (*prendre en compte la nouvelle offre ...*)
                                    then (Validee, tId2, pClient, x)#s  (*...et la valider*)
                                    else (etat, tId2, pClient, x)#s) (*...sans la valider*)
                                else (etat, tId2, pClient, pMarchand)#s (*ignorer l'offre*)
                                ) |
                      Annulee \<Rightarrow> (etat, tId2, pClient, pMarchand)#s |
                      PrixProposer \<Rightarrow> (if x > 0
                                    then
                                        (if pClient \<ge> x (*prendre en compte la nouvelle offre ...*)
                                        then (Validee, tId2, pClient, x)#s  (*...et la valider*)
                                        else (EnCours, tId2, pClient, x)#s (*...sans la valider*)
                                        )
                                    else (etat, tId2, pClient, pMarchand)#s (*ignorer l'offre*)
                                ) |
                      PrixDemander \<Rightarrow> (if (x < pMarchand)  \<and> (x > 0)
                                then (etat, tId2, pClient, x)#s
                                else (etat, tId2, pClient, pMarchand)#s
                               ))
                    else (etat,tId2, pClient, pMarchand)#(offreMarchand tId1 x s)
                  )"

(*Traitement des messages *)
fun traiterMessage::"message \<Rightarrow> transBdd \<Rightarrow> transBdd"
where
"traiterMessage (Pay tId x) bdd = (offreClient tId x bdd)" |
"traiterMessage (Ack tId x) bdd = (offreMarchand tId x bdd)" |
"traiterMessage (Cancel tId) bdd = (annuleOffre tId bdd)"

(*Construit la liste des transactions validées *)
fun export::"transBdd \<Rightarrow> transaction list"
where
"export [] = []" |
"export ((etat, tId, pClient, pMarchand)#s) = (if (etat = Validee)
                                              then ((tId, pClient)#(export s)) 
                                              else (export s)
                                          )"

fun reverse::"'a list \<Rightarrow> 'a list"
where
"reverse [] = []" |
"reverse (x#xs) = ((reverse xs)@[x])"

(*Permet de traiter une liste de message *)
fun traiterMessageList2::"message list \<Rightarrow> transBdd"
where
"traiterMessageList2 [] = []" |
"traiterMessageList2 (m#s) = (traiterMessage m (traiterMessageList2 s))"

fun traiterMessageList::"message list \<Rightarrow> transBdd"
where
"traiterMessageList l= (traiterMessageList2 (reverse l))"

lemma lemme1 : "(List.member (export (traiterMessageList l)) (tid, val)) \<longrightarrow> (val > 0)"
quickcheck [tester=narrowing, timeout=400, size=8]
sorry

fun tidIsKeyAux::"transBdd \<Rightarrow> transid list \<Rightarrow> bool"
where
"tidIsKeyAux [] _ = True" |
"tidIsKeyAux ((e, tId, pClient, pMarchand)#xs) ltId = (\<not>(List.member ltId tId) \<and> tidIsKeyAux xs (tId#ltId))"

fun tidIsKey::"transBdd \<Rightarrow> bool"
where
"tidIsKey bdd = (tidIsKeyAux bdd [])" 

lemma lemme2 :"bdd=(traiterMessageList l) \<longrightarrow> (tidIsKey bdd)"
quickcheck [tester=narrowing, timeout=200, size=6]
sorry

fun getTidInBDD :: "transid \<Rightarrow> transBdd \<Rightarrow> trans"
where 
"getTidInBDD tId1 ((etat, tId2, pClient, pMarchand)#s) = 
  (if (equal tId1 tId2)
    then (etat, tId2, pClient, pMarchand)
    else (getTidInBDD tId1 s))"

fun tidInBDD :: "transid \<Rightarrow> transBdd \<Rightarrow> bool"
where 
"tidInBDD _ [] = False" |
"tidInBDD tId1 ((etat,tId2, pClient, pMarchand)#s) = 
  (if (equal tId1 tId2)
    then True
    else((tidInBDD tId1 s)))"

value "(getTidInBDD (0,0,0) (traiterMessageList [(Cancel (0,0,0))]))"

lemma lemme3 :"(bdd=(traiterMessageList (l@[(Cancel tid)]))) \<longrightarrow> (let (e, _, _, _)=(getTidInBDD tid bdd) in (e=Annulee))"

quickcheck [tester=narrowing, timeout=200, size=5]
sorry

lemma lemme4 :"(bdd=(traiterMessageList (l@[(Cancel tid)]@l2)) \<and>
                (tidInBDD tid bdd)) \<longrightarrow> (let (e, _, _, _)=(getTidInBDD tid bdd) in
                                          e = Annulee)"
quickcheck [tester=narrowing, timeout=200, size=4]
sorry
    
lemma lemme5 : "let bdd=(traiterMessageList l) in 
  (((List.member l (Pay tid m))\<and> (List.member l (Ack tid n)))\<and>(m\<ge>n)\<and>(m>0) \<and> \<not>(List.member l (Cancel tid)))
  \<longrightarrow> (let (e, tid2, offre, d)=(getTidInBDD tid bdd) in e = Validee)"
quickcheck [tester=narrowing, timeout=200, size=6]
sorry

lemma lemme6 : 
"\<exists>m.((bdd=traiterMessageList l) \<and>  (List.member (export bdd) (tid, m))\<longrightarrow>(\<exists>n.(List.member l (Pay tid m))\<and>(List.member l (Ack tid n))
  \<and>(m\<ge>n)))"
quickcheck [tester=narrowing, timeout=200, size=6] 
sorry  

lemma lemme7Client : "((bdd=(traiterMessageList (l@[(Pay tid m1)]@l2@[(Pay tid m2)]@l3)))\<and>(m1>m2)) \<longrightarrow> 
  (let (_, _, m, _) = (getTidInBDD tid bdd) in (m=m1))"
quickcheck [tester=narrowing, timeout=200, size=4]
sorry
 

lemma lemme7Marchand : "((bdd=(traiterMessageList (l@[(Ack tid m1)]@l2@[(Ack tid m2)]@l3)))\<and>(m1<m2)) \<longrightarrow> 
  (let (_, _, _, m) = (getTidInBDD tid bdd) in (m=m1))"
quickcheck [tester=narrowing, timeout=200, size=4]
sorry

lemma lemme8 : "((bdd=(traiterMessageList l)) \<and> (List.member bdd (Validee, tid, m, offre))) \<longrightarrow>
  (let bdd2=(traiterMessage mess bdd) in ((List.member bdd2 (Validee, tid, m, offre))\<or> (List.member bdd2 (Annulee, tid, m, offre))))"
quickcheck [tester=narrowing, timeout=200, size=6]
sorry

lemma lemme9 : "((bdd=(traiterMessageList l))\<and>(lValid=(export bdd))\<and>(List.member lValid (tid,M))
                \<longrightarrow> (let (_, _, mc, _)=(getTidInBDD tid bdd) in (M=mc)))"
 
quickcheck [tester=narrowing, timeout=200, size=6]

sorry

value "traiterMessageList [(Ack (0, 0, 0) 0), (Pay (0, 0, 0) 1), (Pay (0, 0, 0) 1)]"

code_include Scala ""
{*// Code generated by Isabelle - DO NOT EDIT
package validator.GestinSinquinRenault
*}
export_code traiterMessageList export in Scala 

end

