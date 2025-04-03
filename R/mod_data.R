#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import shinyalert
#' @import shinybusy
#' @import sf
#' @import terra
#' @import shinythemes
#' @import bslib
#' @import bsicons
#' @import leaflet
#' @import shinydashboard
mod_data_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Naturverdier"),
    br(),
    fluidRow(

             bslib::value_box(
               title = "",
               value = "",
               h3("Her kan du se ulike kart som viser forekomsten av viktig natur. Hvis et prosjekt ligger innenfor et av disse områdene, er prosjektet merket som «risikolokalitet for tap av viktig natur». Det er imidlertid viktig å merke seg at dette er ikke nødvendigvis i samsvar med kommunale, gjeldende byggeforskrifter."),
               theme = bslib::value_box_theme(bg = "white", fg = "black"),
               showcase= bsicons::bs_icon("book"),
             )
    ),#/row1,
    br(),
    fluidRow(
      column(6,
             selectInput(
               ns("layer_select"),
               "Velg et kartlag for viktig natur",
               choices = c(
                 "",
                 "Naturvernområder" = "nat_vern",
                 "Myrområder" = "myr",
                 "Villreinområder" = "rein",
                 "Pressområder i strandsonen"="press_strand",
                 "Inngrepsfri natur"="inon",
                 "Vassdragsnatur"="water",
                 "Utvalgte og truede naturtyper" = "nat_ku",
                 "Friluftslivsområder" = "friluft",
                 "Naturskog" = "forest"),
               selected = ""
             )
             ),
      column(6,
             tags$hr(),
             tags$head(tags$style(HTML("
      #layer_image img { max-width: 100%; height: 100%; }
    "))),
             imageOutput(ns("layer_image"), height = "auto"),
             br(),
             textOutput(ns("layer_description_short"))),

    ),
    br(),
    fluidRow(
      leaflet::leafletOutput(ns("data_map"))
    ),
    br(),
    fluidRow(
              shinydashboard::box(title = "Litt mer vitenskapelig forklart",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 #includeHTML("nature_types_text/utv_true_nat.html"),
                                 textOutput(ns("layer_description_long")),
                                 collapsed = TRUE,
                                 width = 12),
             shinydashboard::box(title = "Mer informasjon ",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 textOutput(ns("layer_mer")),
                                 collapsed = TRUE,
                                 width = 12)
    )#/row2


  )
}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, adm_unit, in_files){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

      output$title<-renderUI({
      h2(paste0("Naturverdier i ", as.character(adm_unit)," kommune"))
    })


      kom_dat<-in_files$kom_dat
      bbox<-in_files$bbox
      myr_dat<-in_files$myr
      myr_dat[myr_dat != 1] <- NA

    wms_url <- list(
      "inon" = "https://kart.miljodirektoratet.no/geoserver/inngrepsfrinatur/wms",
      "press_strand"="https://wms.geonorge.no/skwms1/wms.spr_strandsoner?service=wms&request=getcapabilities",
      "nat_vern"= "https://kart.miljodirektoratet.no/arcgis/services/vern/mapserver/WMSServer",
      "rein" = "https://kart.miljodirektoratet.no/arcgis/services/villrein/mapserver/WMSServer",
      "nat_ku" = "https://kart.miljodirektoratet.no/arcgis/services/naturtyper_kuverdi/mapserver/WMSServer",
      "friluft" = "https://kart.miljodirektoratet.no/arcgis/services/friluftsliv_kartlagt/mapserver/WMSServer",
      "forest" = "https://image001.miljodirektoratet.no:443/arcgis/services/naturskog/naturskog_v1/MapServer/WMSServer"
    )

    legend_url <- list(
      "inon" = "https://kart.miljodirektoratet.no/geoserver/inngrepsfrinatur/wms?request=GetLegendGraphic&version=1.1.1&format=image%2Fpng&width=20&height=20&layer=status",
      "press_strand"="https://wms.geonorge.no/skwms1/wms.spr_strandsoner?version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=spr_strandsoner&format=image/png&STYLE=default",
      "nat_vern" = "https://kart.miljodirektoratet.no/arcgis/services/vern/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=naturvern_klasser_omrade",
      "rein" = "https://kart.miljodirektoratet.no/arcgis/services/villrein/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=villrein_leveomrade",
      "nat_ku" = "https://kart.miljodirektoratet.no/arcgis/services/naturtyper_kuverdi/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=kuverdi_naturtype_alle",
      "friluft"="https://kart.miljodirektoratet.no/arcgis/services/friluftsliv_kartlagt/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=friluftsliv_kartlagt_verdi",
      "forest" = "https://image001.miljodirektoratet.no:443/arcgis/services/naturskog/naturskog_v1/MapServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=skog_etablert_foer_1940_ikke_flatehogd"
    )
    layer <-list(
      "inon" = "status",
      "press_strand" = "spr_strandsoner",
      "nat_vern" = "naturvern_klasser_omrade",
      "rein" = "villrein_leveomrade",
      "nat_ku" = "kuverdi_naturtype_alle",
      "friluft" = "friluftsliv_kartlagt_verdi",
      "forest" = "skog_etablert_foer_1940_ikke_flatehogd"
    )
    attr_list <-list(
      "inon" = "@Milj.dir. Geoserver",
      "press_strand" = "@Geonorge",
      "nat_vern"= "@Milj.dir. Arcgis",
      "rein"= "@Milj.dir. Arcgis",
      "nat_ku" = "@Milj.dir. Arcgis",
      "friluft" = "@Milj.dir. Arcgis",
      "forest" = "@Milj.dir. Arcgis"
    )

    # Placeholder for layer descriptions
    layer_description_short <- list(
      "nat_vern" = "Et verneområde er et område på land eller i vann hvor naturen skal vernes mot menneskelig inngrep og større forstyrrelser. I verden og i Norge er arealendringer gjennom menneskelige inngrep den største årsaken til mindre biologisk mangfold. Naturmangfoldloven definerer hvordan de forskjellige typene naturvernområder er beskyttet og hvilken aktivitet er tillat eller forbudt. (Beskriv de ulike typene og forskjellen på vern av disse) (Hvor mye av Norge er i et verneområde) I Norge finnes det mer enn 3300 verneområder som verner 17,7 % av Norges fastland og 4.5% av sjøarealet.",
      "nat_ku" = "Før en utbygging av f.eks. veier, større bygninger eller næringsområder skal utbygger gjøre en konsekvensvurdering for påvirkning på ulike naturtyper. Miljødirektoratet har samlet de viktigste naturtypene i et kart for å gjør det enklere å gjøre en konsekvensvurdering. Likevel viste NRK-saken Norge i rødt, hvitt og grått at vi fortsatt bygger på truet natur og natur som er sentrale levesteder for truede arter. Eksempler på truede naturtyper er slåtteeng, kystgranskog, grisehalekorallbunn og kalksjø . Nesten halvparten av alle truede arter har sitt leveområde helt eller delvis i skog",
      "press_strand" = "«Strandsonen» er områder innen 100 meter fra sjøen, og langs større vassdrag. I dag er naturen i strandsonen mange steder under press grunnet  utbygging. Dette påvirker befolkningens mulighet til å bruke strandsonen til friluftsliv, og det kan i tillegg ramme planteliv og dyreliv. En tredjedel av Norges strandsone er påvirket av menneskelig aktivitet (SSB). ",
      "myr" = "En myr er et økosystem med permanent høyt grunnvannsnivå hvor nedbrytingen av plantene som vokser der tar så lang tid at det hoper seg opp med døde planter og det dannes det som kalles torv. Dannelsen av torv tar tusenvis av år, og jo eldre myra er, desto dypere er den. Myra er et svært viktige leveområde for mange arter, både planter, insekter, fugler og andre fyr. Den er også viktig når det kommer til å lagre vann, og er derfor en drikkevannskilde og demper faren for flom.",
      "inon" = "Inngrepsfri natur er natur som i luftlinje ligger én kilometer eller mer unna større menneskelige naturinngrep. Menneskelige naturinngrep er for eksempel veier, bygninger og energi- og næringsanlegg. Inngrepsfri natur er viktige leveområder for en del dyrearter, og de er viktige for mangfoldet av natur- og friluftsområder i Norge. I de siste hundre årene har inngrepsfri natur gått fra å dekke halvparten av Norges areal til i dag å dekke litt over 10 %. Den viktigste årsaken til at det i dag forsvinner inngrepsfri natur er på grunn av energiutbygging gjennom vind- og vannkraft. Gjennom naturrestaurering kan inngrepsfri natur øke, hvor da inngrepene fjernes og de samme arealene blir tilbakeført til naturen. Hjerkinn skytefelt på Dovre er et eksempel på hvor naturrestaurering har ført til en økning i inngrepsfri natur.",
      "rein" = "Villrein er et norsk symbol for landets unike dyreliv og natur. Tidligere hadde villrein sin utbredelse over hele Norge, hvor den da kunne vandre fritt og brukte ulike områder til ulike sesonger. Menneskelig aktivitet, slik som friluftsliv og utbygging, og klimaendringer har satt villreins leveområder under stort press. For å ta vare på villreinen og for å sikre dens leveareal, har forvaltningen laget noe som kalles «villreinområder». De omtrent 20 000 villreinene vi har i Norge lever i dag i de 24 nasjonale villreinområdene som alle befinner seg i Sør-Norge. Villreinområdene er ikke koblet sammen, og villreinen forflytter seg derfor lite mellom de ulike områdene.",
      "water" = "Vassdragsnatur defineres som områdene av land ved vassdrag, innenfor en 100 meter bred buffer. Disse områdene er avgjørende for helsen til både vassdraget og det omkringliggende landskapet. Vassdragsnatur er hjem til mange planter, dyr og mikroorganismer, og fungerer som en grønn korridor som kobler sammen forskjellige habitater og lar dyrelivet bevege seg mellom områder. Plantene i disse områdene hjelper til med å rense vannet ved å filtrere ut forurensninger og næringsstoffer som er til overs. De forhindrer også jorderosjon ved å holde sammen jorden med røttene sine og bidrar til å redusere flomrisikoen ved å ta opp overskuddsvann. Samtidig er flom i disse sonene gunstig fordi det bringer friske næringsstoffer til jorden. Vassdragsnatur står overfor press fra menneskelige aktiviteter som nedbygging, forurensning, landbruk og flomvernstiltak som flomvoller, som kan forstyrre den naturlige vannbevegelsen og hindre dyrelivets bevegelse. Å beskytte vassdragsnatur sikrer rent vann, støtter biologisk mangfold og gir mange fordeler for både naturen og mennesker.",
      "friluft" = "Viktige og svært viktige friluftslivsområder er områder som har stor verdi for friluftsliv for mennesker. Her kan folk utøve fysisk aktivitet, avslapping og å være med andre mennesker. Dette gjør at områdene har en viktig verdi for både fysisk og psykisk helse. Friluftslivsområder kan være turområder i by og bygd, lekeområder og stranda",
      "forest" = "Hva en naturskog er kan forklares på mange måter. Enkelt sagt er naturskog skog hvor det har vært så lite menneskelig påvirkning at skogens naturlige prosesser styrer. Én måte vi kan kartlegge hvor i Norge vi har slik skog er ved å finne områder hvor det er lenge siden skogen har blitt flatehogd (altså hvor nesten alle trær i skogen er hugget ned). Da vil ofte skogen være så lite påvirket av mennesker at trærne i skogen har ulik alder med mange gamle trær, og det er døde trær som er brutt ned til ulik grad. Naturskog er viktig for truede arter, hvor både gamle og døde trær er hjem og matkilde for arter man ikke vil kunne finne andre steder. ."
    )

    layer_description_long <- list(
      "nat_vern" = "For å opprettholde og forbedre de viktige økosystemtjenestene og økosystemtilstandene på lang sikt, opprettes det nasjonalparker og naturreservater. Disse har som mål å styrke det biologiske mangfoldet, beskytte og bevare verdifulle naturmiljøer og restaurere naturlige leveområder for truede arter. Vern, bevaring og restaurering av natur skal motvirke tilbakegang av leveområder og dermed tilbakegang av dyre- og plantearter. Den internasjonale naturvernunionen (International Union for Conservation of Nature – IUCN) utvikler globale kriterier og standarder for å inndele verneområder og definere vernetiltak for å ta vare på naturen. Den internasjonale klassifikasjon er litt forskjellige fra det norske systemet. I Norge finnes det mer enn 3300 verneområder som verner 17,7 % av fastlandet i Norge og 17 marine områder som verner 4.5% sjøarealet innenfor Norges territoriale grense. Hovedsakelig vernet er høyfjellsområder , mens skog, hav- og kystområder er underrepresentert. Vi ser også at over hele verden er særlig områder som har lavere økonomisk verdi blir vernet. Dette resulterer i vern av arealer som ligger i mer avsidesliggende områder med lite menneskelig påvirkning, men som ikke nødvendigvis beskytter det som er verdifullt med tanke på det biologiske mangfoldet.",
      "nat_ku" = "«Naturtyper av forvaltningsinteresse» er et samlebegrep om naturtyper som tillegges vekt av forvaltningen ved behandling av konsekvensutredninger (KU). Det er naturtyper som selv er truet eller utvalgt fordi de er leveområder for truede arter.  I NRK saken ble det kalt også «utvalgte eller rødlistede naturtyper». Rødlister er laget for arter og naturtyper som er truet i Norge. Dette består av en rekke naturtyper som er spesielt viktige av ulike grunner:
•	naturtyper som Miljødirektoratet har valgt ut for kartlegging fordi de enten er truet eller nær truet
•	naturtyper som er antatt å være spesielt viktig i biologisk mangfold fra eldre kartlegginger
•	naturtyper som dekker sentrale økosystemfunksjoner, for eksempel fordi de er levesteder for truede og nær truede arter
•	naturtyper som er spesielt dårlig kartlagt
",
      "press_strand" = "Strandsonen er definert som «areal 100 meter vinkelrett inn over land fra kystlinjen, målt i horisontalplanet. Denne sonen er omfattet av et generelt byggeforbud etter plan- og bygningsloven §1-8.» Strandsonen inneholder en stor variasjon av ulike naturtyper og et stort og variert artsmangfold. Områdene langs kysten, sjøer og elver  tilbyr viktige habitater for planter, fugler, insekter, amfibier og mikroorganismer (se vassdragsnatur). De fungerer også som viktige blå-grønne korridorer for biologisk mangfold, og forbinder ulike naturlige habitater.
Mange verdsetter utsikt til vann, og på grunn av naturskjønnhet og naturmangfold er sonene nær sjøen og langs vann og vassdrag populære for ulike fritids- og rekreasjonsaktiviteter.
I tillegg til de ulike miljø og brukerinteressene i strandsonen, forventes havnivåstigning og økt hyppighet av flom med klimaendringene. Dermed er det flere brukerinteresser i strandsonen som må balanseres og planlegges på en langsiktig måte. Arealplanlegging skal ta vare på strandsonenes miljøverdier og allmenne interesser.
Generelt er det byggeforbud i 100-metersbeltet langs sjøen over hele landet. Kommunen kan likevel åpne for bygging når det er sterke interesser som taler for det.  Kommunen kan regulere hva slags arealbruk som er tillatt i strandsonen. Kommunene må også ivareta de ulike miljø-, klima og samfunnsinteressene i til strandsonene i sine planbestemmelser med konkrete tiltak.
",
      "myr" = "Forutsetningen for at en myr skal kunne oppstå, er at jorda er mettet med vann, hvor vannet fortrenger luften og dermed oksygenet. Ettersom nedbryting av plantemateriale (også kalt biomasse) krever oksygen, blir veldig lite av biomassen brutt ned. I en myr er tilførselen av ny biomasse større enn nedbrytingen og tapet av gammel biomasse, og gammel biomasse vil derfor hope seg opp og danne det som kalles torv. Over tid vil det bli så mye torv at plantene som vokser på toppen av myra ikke har tilgang på grunnvannet. Disse plantene får derfor kun tilgang på vann fra regnvann, og kalles nedbørsmyr. Myrer som har tilgang på vann fra grunnvannet kalles jordvannsmyr. Regnvann gir ofte lite næring til plantene, og nedbørsmyrer er derfor også artsfattige. Grunnvann gir derimot tilførsel av mer næring, og jordvannsmyrer har derfor flere arter enn nedbørsmyrer. Myrer kan videre også kategoriseres i ulike torvmarksformer avhengig av hvordan strukturen på myra er slik som f.eks. høymyr, bakkemyr og strengmyr. Myra bidrar med en rekke økosystemtjenester, slik som opprettholdelse av vann- og næringsstoffbalanse, lokal og global klimaregulering samt gunstige leveområder for fugler, insekter og planter. Myra har en positiv global klimaeffekt gjennom lagring av store mengder karbon. Den er derfor et viktig bindeledd mellom natur, biologisk mangfold og klima. Et godt tiltak for klima- og naturkrisen er derfor å beskytte myrområder.",
      "inon" = "Inngrepsfri natur er defineres av én inngrepsparameter og tre ulike avstandsparametere. Inngrepsparameteren defineres som kartlagt tekniske inngrep i naturlig arealdekke (Inngrepsfrie naturområder i Norge - miljodirektoratet.no). Avstandsparameterene er inndelt i tre soner: 1) Villmarkspregede områder ligger fem kilometer eller mer fra tyngre tekniske inngrep, 2) inngrepsfri sone 1 ligger mellom tre og fem kilometer, og 3) inngrepsfri sone 2 ligger mellom én og tre kilometer fra tyngre tekniske inngrep. Det er viktig at inndelingen er bare avstandsbasert og gir ingen informasjon om kvalitet og funksjonalitet av naturen. Det er derfor ikke mulig å si om inngrepsfri naturområders økosystemkvalitet, naturmangfold eller økosystemtjenester. Utbygging kan føre til en redusering eller fragmentering av urørte områder. Med de overordnede nasjonale miljømålene er statens forventninger til regional og kommunal planlegging følgende:
•	legge til rette for en bærekraftig utvikling i fjell og utmark og har særlig oppmerksomhet rettet mot områder med stort utbyggingspress. Dette skjer blant annet ved å fastsette langsiktige utbyggingsgrenser.
•	identifisere og ta hensyn til viktig naturmangfold, friluftslivsområder, overordnet grønnstruktur, kulturhistoriske verdier, kulturmiljø og landskap i planleggingen. Samlede virkninger tas hensyn til.
Med kommuneplan ivaretar kommunene størrelse av inngrepsfri natur med en vurdering av hvor og når det kan skje hvilken utbygging. Arealformål, Planbestemmelser, Reguleringsplaner
",
      "rein" = "Villreinområder definerer 24 områder som forvaltes av forvaltningsenheter. Inndelingen er basert på biofysisk forutsetninger som topografi, arealdekke og klimaforutsettinger, infrastruktur mønster og forvaltningsenheter. Fra de 24 villreinområder er ti nasjonale områder og de er inndelt i to europeiske villreinregioner (Villreinområder | Villrein). Forvaltning av villreinområder inneholder regulere og opprettholde bestanden og genetisk mangfold gjennom villreinjakt, gjennomføre overvåking av villrein bestand og kvalitet. Fylker og kommuner er ansvarlig for å hindre tap på villreinområder gjennom arealplanlegging med bruk av plan- og bygningsloven.",
      "water" = "Vassdragsnatur refererer til soner med landareal som ligger ved siden av elver, bekker og innsjøer, og som er avgjørende for helsen til både vassdraget og det omkringliggende landskapet. Disse områdene gir mange økosystemtjenester og fungerer som grønne korridorer som støtter biologisk mangfold. Vassdragsnatur inkluderer kantsonen, et økosystem som påvirkes av tilstedeværelsen av vann og typisk er preget av spesifikke plante- og dyresamfunn som er tilpasset våte forhold. Bredden på kantsonene langs et vassdrag avhenger av faktorer som topografi, størrelsen på vassdraget og hvor ofte området oversvømmes.
Disse områdene støtter mangfoldige økosystemer ved å tilby viktige habitater for en rekke arter, inkludert planter, fugler, insekter, amfibier og mikroorganismer. De fungerer også som viktige grønne korridorer for biologisk mangfold, og forbinder ulike naturlige habitater, slik at arter kan bevege seg mellom områder, finne mat og hekke. Disse områdene bidrar til klimaregulering ved å lagre karbon i form av vegetasjon og jord. Vegetasjonen i disse sonene stabiliserer jorden, forhindrer erosjon ved å binde jorden med røttene, og hjelper med å filtrere ut forurensninger, næringsstoffer og sedimenter før de når vannet. Kantvegetasjon bidrar også til å regulere vanntemperaturen ved å gi skygge, noe som er spesielt viktig for å opprettholde sunne akvatiske habitater.
Under flomhendelser hjelper kantsonene med å absorbere overskuddsvann, redusere flomrisiko og fylle på jorda rundt med næringsstoffer. Flomvernstiltak som diker og lignende flomvernstrukturer forstyrrer vann- og næringsstrømmen mellom elven og dens oversvømmelsesområde, noe som forringer kantsonene og øker flomrisikoen i nedstrømsområder. Urbanisering, landbruk, forurensning og avskoging kan påvirke disse økosystemene, redusere deres evne til å filtrere vann, hindre erosjon og støtte biologisk mangfold. Beskyttelse av kantsoner er avgjørende for å opprettholde vannkvaliteten, hindre jorderosjon, støtte biologisk mangfold og sikre at de fortsetter å levere viktige økosystemtjenester.
",
      "friluft" = "Verdsetting av friluftslivsområder bærer stor betydning for kommunale beslutningsprosesser og fungerer som et verktøy for å balansere ulike sektorinteresser. Verdsettelse er basert på dette kriteriet – brukerfrekvens, regionale og nasjonale brukere, opplevelsesmessige kvaliteter, symbolverdi, funksjon, egnethet, tilrettelegging, kunnskapsverdier, intervensjon, omfang, tilgjengelighet, gode omgivelser, og potensiell bruk. Friluftslivsområder klassifiseres under nærturterreng, leke- og rekreasjonsområde, grønnkorridor, marka, strandsone med tilhørende sjø og vassdrag, jordbrukslandskap, utfartsområde, store turområder med tilrettelegging, store turområder uten tilrettelegging særlige kvalitetsområder, og andre friluftslivsområder.
Friluftslivet har en verdi gjennom den umiddelbare gleden ved aktiviteten, følelsen av mestring, naturopplevelsen og muligheten for fysisk aktivitet, rekreasjon, avkobling og sosialt samvær. Dette innebærer at friluftslivet har flere direkte nytteverdier, som forbedret helse og økt livskvalitet. Friluftslivspolitikken har lenge hatt som nasjonalt mål at alle skal ha tilgang til friluftsliv i både nærmiljøet og naturen generelt. Derfor bør det være en selvfølge at kommuner har et bevisst forhold til sine friluftslivsområder.
Jo større et urbant naturområde er, desto høyere er som regel dets rekreasjonsverdi. Det er imidlertid bemerkelsesverdig at selv relativt små naturområder i bysentrum kan ha stor betydning for rekreasjon
",
      "forest" = "Det finnes mange definisjoner på hva en naturskog er, men den prinsipielle definisjonen er at naturskog er skog hvor menneskelig påvirkning har vært minimal over lang tid eller hvor påvirkningen har vært så liten at skogens struktur, sammensetning og økologiske prosesser er det som dominerer skogen. Denne definisjonen kan ikke brukes for å kartlegge hvor vi har naturskog i Norge, og det er derfor laget tre kart med ulike definisjoner av naturskog: 1) skog etablert før 1940 (ikke flatehogd siden), 2) naturskogsannsynlighet, og 3) naturskogsnærhet. I det første kartet er naturskog skog som ikke har vært flatehogd etter 1940. Flatehogst er hvor nesten alle trær i en skog hogges ned. I det andre kartet er naturskog også skog som er etablert før 1940, men i tillegg er det ikke gjort noen inngrep eller behandling av skogen etter 1965. I det tredje og siste kartet beskrives graden en skog ligner en naturskog, og for å gjøre dette bruker man variabler slik som dødvedandel, dødvedvariasjon og hvilket suksesjonsstadie skogen er i. Ifølge de tre kartene har vi 35, 44 eller 13 % naturskog i Norge.
Naturskog er viktig fordi veldig mange rødlistede arter finnes i slike skoger. Gamle trær gir habitater for dyr og planter. Døde trær blir til stående og liggende død ved (gadd og læger) som brytes ned av sopp og insekter som er spesialister på nedbryting, og som derfor vil kun finnes i skog med dødved. Naturskogen har mye karbon lagret i både trærne og i jorda, og uforstyrret naturskog kan derfor være positivt for klimaet."
    )

    layer_mer <- list(
      "nat_vern" = "Easy beskrivelse for Naturvernområder data layer.",
      "nat_ku" = "Easy beskrivelse for Naturvernområder data layer.",
      "press_strand" = "Easy beskrivelse for pressområder i strandsonen data layer.",
      "myr" = "Easy beskrivelse for myr data layer.",
      "inon" = "Easy beskrivelse for inngrepsfri natur data layer.",
      "rein" = "Easy beskrivelse for villreinområder data layer.",
      "water" = "Easy beskrivelse for vassdragsnatur data layer.",
      "friluft" = "Easy beskrivelse for verdsatte friluftsliv områder data layer.",
      "forest" = "Easy beskrivelse for vernskog data layer."
      )



    # Placeholder for layer images
    layer_images <- list(
      "nat_vern" = "layer_pictures/vern.jpg",
      "nat_ku" = "layer_pictures/utv_trued_nattype.png",
      "friluft" = "layer_pictures/friluft.jpg",
      "inon" = "layer_pictures/inon.jpg",
      "myr" = "layer_pictures/myr.jpg",
      "press_strand" = "layer_pictures/press_strand.jpg",
      "water" = "layer_pictures/vassdrag.jpg",
      "forest" = "layer_pictures/vernskog.jpg",
      "rein" = "layer_pictures/villrein.jpg"
    )

    # Render Leaflet map
    output$data_map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(data= kom_dat,color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 10)
    })

    # Observe layer selection
    observe({
      req(input$layer_select)
      selected_layer <- input$layer_select
      show_modal_spinner(text = "hent data", color = "green")
      if(selected_layer == "water"){
        leaflet::leafletProxy("data_map") %>%
          leaflet::clearTiles()%>%
          leaflet::clearControls()%>%
          addWMSTiles(
            baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
            layers = "norges_grunnkart",
            options = WMSTileOptions(format = "image/png", transparent = TRUE),
            attribution = "© Kartverket",
            group = "Kartverket basiskart"
          )%>%
          leaflet::addTiles(
            urlTemplate = "https://maps-test.nina.no/titiler/cog/tiles/WebMercatorQuad/{z}/{x}/{y}@1x?colormap_name=blues&rescale=0%2C1&bidx=1&url=https%3A%2F%2Fmaps-test.nina.no%2Fmedia%2Fmaps%2Fsources%2F37%2Fvassdrag_100m.tif.cog",
            options = tileOptions(tms = FALSE, opacity = .6)  # Keep tms=FALSE for Web Mercator Quad tiles
          )


      }else if(selected_layer == "myr"){

        leaflet::leafletProxy("data_map") %>%
          leaflet::clearTiles()%>%
          leaflet::clearControls()%>%
          addWMSTiles(
            baseUrl = "https://wms.geonorge.no/skwms1/wms.norges_grunnkart?service=wms&request=getcapabilities",
            layers = "norges_grunnkart",
            options = WMSTileOptions(format = "image/png", transparent = TRUE),
            attribution = "© Kartverket",
            group = "Kartverket basiskart"
          )%>%
          addRasterImage(myr_dat, colors = "blue", opacity = .6)

      }else{

        wms_selected <- wms_url[[selected_layer]]
        legend_selected <-legend_url[[selected_layer]]
        layer_selected <-layer[[selected_layer]]
        attribution <- attr_list[[selected_layer]]

        if(is.null(wms_selected) | is.null(legend_selected)){
          #empty leaflet proxy map
          leaflet::leafletProxy("data_map")%>%
            leaflet::clearTiles()%>%
            leaflet::clearControls()%>%
            leaflet::addTiles()%>%
            leaflet::setView(lng =mean(bbox$xmin,bbox$xmax) , lat = mean(bbox$ymin,bbox$ymax), zoom = 10)
        }else{
          leaflet::leafletProxy("data_map") %>%
            leaflet::clearTiles()%>%
            leaflet::clearControls()%>%
            leaflet::addTiles() %>%
            leaflet::addWMSTiles(
              baseUrl = wms_selected,
              layers = layer_selected,
              options = leaflet::WMSTileOptions(
                format = "image/png",
                transparent = TRUE
              ),
              attribution = attribution,
              group = legend_selected
            ) %>%

            # # # Add a layer control to toggle the WMS layer
            # leaflet::addLayersControl(
            #   baseGroups = c("Base Map"),
            #   overlayGroups = layer_selected,
            #   options = leaflet::layersControlOptions(collapsed = FALSE)
            # ) %>%

            # Add the legend manually
            leaflet::addControl(html = paste0("<img src='", legend_selected, "' style='width:250px;'>"),
                                position = "bottomright")
        }

      }
      remove_modal_spinner()



    })

    # Update image and description based on selected layer
    output$layer_image <- renderImage({

      req(input$layer_select)
      selected_layer <- input$layer_select
      image_path <- system.file("extdata", layer_images[[selected_layer]], package = "eikaCAN")

      list(
        src = image_path,
        contentType = "image/jpeg",
        alt = "Image from extdata"
      )


    }, deleteFile = FALSE)

    output$layer_description_short <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_description_short[[selected_layer]]
    })

    output$layer_mer <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_mer[[selected_layer]]
    })

    output$layer_description_long <- renderText({
      req(input$layer_select)
      selected_layer <- input$layer_select
      layer_description_long[[selected_layer]]
    })

  })
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")
