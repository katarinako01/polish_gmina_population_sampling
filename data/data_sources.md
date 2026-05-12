# Data Sources

All datasets were downloaded from the **GUS Local Data Bank**  
(Statistics Poland), freely available at [bdl.stat.gov.pl](https://bdl.stat.gov.pl).  
No registration is required. 

---

## Population — Census 2021 (`NARO_4181_CTAB_20260508153506.csv`)

**Variable:** Total resident population per gmina, as of 31 March 2021  
**Source:** Statistics Poland, National Census of Population and Housing 2021  
**Category:** National Censuses → Census 2021 – Population → Population by age and sex
**Role in study:** Target variable

**Navigation path:**

```
DATA
└── Data by areas
    └── National Censuses
        └── Census 2021 – Population
            └── Population by age and sex
                ├── Year: 2021
                ├── Sex: total
                ├── Age: total
                ├── Territorial unit: administrative units (gmina level)
                ├── Add all to selection
                └── Export → CSV – multidimensional table
```

---

## Dwelling Stock — GUS Housing Statistics (`GOSP_2166_CTAB_20260511201617.csv`)

**Variable:** Total number of dwellings per gmina, as of 2021  
**Source:** Head Office of Geodesy and Cartography (GUGiK) via GUS Local Data Bank  
**Category:** Housing economy and municipal infrastructure → Dwelling stock
**Role in study:** Auxiliary variable

**Navigation path:**

```
DATA
    └── Data by areas
        └── Housing economy and municipal infrastructure
            └── Dwelling stock
                └── Dwelling stocks
                ├── Year: 2021
                ├── Location: total
                ├── Dwelling stocks: dwellings
                ├── Territorial unit: administrative units (gmina level)
                ├── Add all to selection
                └── Export → CSV – multidimensional table
```

## Area (`PODZ_1410_CTAB_20260508120159.csv`)

**Variable:** Total area in km² per gmina, as of 2021  
**Source:** Head Office of Geodesy and Cartography (GUGiK) via GUS Local Data Bank  
**Category:** Territorial Division → Geodetic Area  
**Role in study:** Auxiliary variable candidate (rejected - Pearson correlation with population $r = 0.108$)

**Navigation path:**
```
DATA
└── Data by areas
    └── Territorial Division
        └── Geodetic Area (Data of the Head Office of
            Geodesy and Cartography)
            └── Area
                ├── Year: 2021
                ├── Area: total in square km
                ├── Territorial unit: administrative units (gmina level)
                ├── Add all to selection
                └── Export → CSV – multidimensional table
```

---

## License

All datasets are published by Statistics Poland (GUS) under  
[Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).  
Attribution: Statistics Poland (GUS), [bdl.stat.gov.pl](https://bdl.stat.gov.pl)
