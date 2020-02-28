/* RULES /*

book(illiad,homer,study,500).
book(c,richie,study, 150).
book(ntbible,sams,reference,480).
book(ntfordummies,bob, reference,200).
book(montypython,cleese,comedy,300).
book(pythonalgorithms,david,study, 225).
book(lilacbus,binchey,fiction,200).
book(hamlet,shakespere,drama,450).

/* RULES /*


build_library(Lib) :- findall(book(Title, Author, Genre, Size), book(Title, Author, Genre, Size), Lib).