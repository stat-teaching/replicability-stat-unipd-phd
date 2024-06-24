set.seed(2024)

students <- data.frame(
  stringsAsFactors = FALSE,
  id = c(1L,2L,3L,4L,5L,6L,7L,8L,
         9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,
         20L,21L,22L,23L,24L,25L,26L),
  student = c("Sara Costa","Chuchu Jia",
              "Ludovica Natali","Evgenii Pashnin","Ambra Perugini",
              "Alberto Petrin","Matteo Licitra","Irene Di Pietro",
              "Yunfeng Sun","Allegra Sartore","Runpeng Miao",
              "Jaurel Kagho zanguim","Isabella Valbusa","Elena Baldisseri",
              "Denise Feurer","Abera Agmas Sisay","Carraro Enrico",
              "Cecchetto Alex","Dame Kenenisa Tadesse","Forcina Davide",
              "Genesin Leonardo","Keshwani Vanshika","Muhanguzi Hillary",
              "Murru Virginia","Patalano Maria Francesca",
              "Schiavone Matteo"),
  email = c("sara.costa@unipr.it",
            "chuchu.jia@phd.unipd.it","ludovica.natali@phd.unipd.it",
            "evgenii.pashnin@studenti.unipd.it",
            "ambra.perugini@studenti.unipd.it","alberto.petrin.1@phd.unipd.it",
            "matteo.licitra@phd.unipd.it","irene.dipietro@phd.unipd.it",
            "yunfeng.sun@studenti.unipd.it","allegra.sartore@ubep.unipd.it",
            "runpeng.miao@phd.unipd.it",
            "jaurel.kaghozanguim@studenti.unipd.it","isabella.valbusa@phd.unipd.it",
            "elena.baldisseri@studenti.unipd.it","denise.feurer@ubep.unipd.it",
            "agmassisay.abera@studenti.unipd.it",
            "enrico.carraro.10@studenti.unipd.it","alex.cecchetto@studenti.unipd.it",
            "kenenisatadesse.dame@studenti.unipd.it",
            "davide.forcina@unina.it","leonardo.genesin@studenti.unipd.it",
            "vanshika.keshwani@studenti.unipd.it",
            "hillary.muhanguzi@studenti.unipd.it","virginia.murru@studenti.unipd.it",
            "mariafrancesca.patalano@studenti.unipd.it",
            "matteo.schiavone@studenti.unipd.it"),
  phd_program = c("Psychological Sciences",
                  "Psychological Sciences","Psychological Sciences",
                  "Psychological Sciences","Psychological Sciences",
                  "Psychological Sciences","Molecular Science","Neuroscience",
                  "Neuroscience","Biostatistics",
                  "Materials Science and Engineering","Material Science and Technology",
                  "Human Rights, Society and Multi-level Governance","Biomedical Sciences",
                  "Biostatistics","Statistical Sciences",
                  "Statistical Sciences","Statistical Sciences","Statistical Sciences",
                  "Statistical Sciences","Statistical Sciences",
                  "Statistical Sciences","Statistical Sciences",
                  "Statistical Sciences","Statistical Sciences","Statistical Sciences"),
  phd_group = c("psych/neuro","psych/neuro",
                "psych/neuro","psych/neuro","psych/neuro","psych/neuro",
                "else","psych/neuro","psych/neuro","stat","else",
                "else","else","else","stat","stat","stat","stat",
                "stat","stat","stat","stat","stat","stat","stat",
                "stat"),
  group = c(1L,2L,3L,4L,5L,5L,5L,4L,
            3L,1L,1L,2L,4L,3L,2L,3L,4L,5L,1L,2L,3L,4L,
            5L,1L,2L,3L)
)

shuffle_rows <- function(data){
  data[sample(1:nrow(data)), ]
}

students$phd_group <- factor(students$phd_group, levels = c("psych/neuro", "else", "stat"))
students <- dplyr::arrange(students, phd_group)

students_shuffled <- split(students, students$phd_group) |> 
  lapply(shuffle_rows) |> 
  do.call(rbind, args = _)

rownames(students_shuffled) <- NULL
students_shuffled$group <- students$group
students_shuffled <- dplyr::arrange(students_shuffled, id)
table(students_shuffled$phd_group, students_shuffled$group)

writexl::write_xlsx(students_shuffled, path = "files/students.xlsx")