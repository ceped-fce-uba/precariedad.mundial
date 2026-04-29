# Precariedad Mundial

Este es el repositorio de datos para el procesamiento de encuestas de hogares, en el marco de la línea de trabajo sobre precariedad mundial del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED - IIEP – UBA). El proyecto **Precariedad Mundial** tiene como objetivo aportar argumentos y evidencias empíricas sobre la incidencia de la precariedad laboral a lo largo del mundo, como también fomentar el intercambio sobre criterios y enfoques para procesar estadísticas laborales. Este repositorio presenta un conjunto homogéneo de bases de datos con variables socio-laborales a partir del procesamiento de microdatos de encuestas de hogares de institutos de estadística oficiales de distintos países del mundo. 

Si utilizas información de este proyecto, agradecemos que cites alguna de nuestras publicaciones:

- [*La calidad del empleo en la Argentina reciente: un análisis sobre su relación con la calificación y el tamaño de las unidades productivas en perspectiva comparada* J Graña, G Weksler, F Lastra *Trabajo y Sociedad 38, 423-446*](https://www.unse.edu.ar/trabajoysociedad/38%20GRANA%20ET%20ALT%20La%20calidad%20del%20empleo%20en%20la%20Argentina.pdf)

- [*Calidad del empleo y estructura del mercado de trabajo en América Latina desde una perspectiva comparada* S Fernández-Franco, JM Graña, F Lastra, G Weksler *Ensayos de Economía 32 (61), 124-151*](https://doi.org/10.15446/ede.v32n61.100343)

El siguiente cuadro presenta las bases disponibles con un enlace para la descarga. También se pone a disposición una página con [Aclaraciones metodológicas](./docs/aclaraciones_metodologicas.html) y otra con [Ejemplos de Uso](./docs/ejemplos_de_uso.html).

## Encuestas procesadas

| País           | Encuesta                                                                                  | Años disponibles|
|----------------|-------------------------------------------------------------------------------------------|---------------|
| Argentina      | Encuesta Permanente de Hogares                                                             | [2019](bases_homog/argentina_2019.rds), [2024](bases_homog/argentina_2024.rds)          |
| Bolivia        | Encuesta Continua de Empleo                                                                | [2019](bases_homog/bolivia_2019.rds)          |
| Brasil         | Pesquisa Nacional por Amostra de Domicílios Contínua - PNAD Contínua                       | [2019](bases_homog/brasil_2019.rds), [2024](bases_homog/brasil_2024.rds)          |
| Chile          | Encuesta Nacional de Empleo (ENE) – módulo Encuesta Suplementaria de Ingresos (ESI)        | [2019](bases_homog/chile_2019.rds), [2024](bases_homog/chile_2024.rds)          |
| China          | Chinese Household Income Project (CHIP)                                                    | [2018](bases_homog/china_2019.rds)          |
| Colombia       | Gran Encuesta Integrada de Hogares - GEIH                                                  | [2019](bases_homog/colombia_2019.rds), [2024](bases_homog/colombia_2024.rds)          |
| Costa Rica     | Encuesta Nacional de Hogares                                                               | [2019](bases_homog/costa_rica_2019.rds)          |
| Ecuador        | Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)                                | [2019](bases_homog/ecuador_2019.rds), [2024](bases_homog/ecuador_2024.rds)          |
| El Salvador    | Encuesta Nacional de Hogares                                                               | [2019](bases_homog/el_salvador_2019.rds)          |
| Estados Unidos | Current Population Survey                                                                  | [2018](bases_homog/estados_unidos_2019.rds)          |
| Europa         | Eurostat Labour Force Survey (LFS)                                                         | [2018](bases_homog/europa_2019.rds)          |
| Guatemala      | Encuesta Nacional de Empleo e Ingresos                                                     | [2019](bases_homog/guatemala_2019.rds)          |
| Mexico         | Encuesta Nacional de Ocupación y Empleo                                                    | [2019](bases_homog/mexico_2019.rds)          |
| Paraguay       | Encuesta Permanente de Hogares Continua                                                    | [2019](bases_homog/paraguay_2019.rds)          |
| Perú           | Encuesta Nacional de Hogares                                                               | [2019](bases_homog/peru_2019.rds)          |
| Uruguay        | Encuesta Continua de Hogares                                                               | [2019](bases_homog/uruguay_2019.rds), [2024](bases_homog/uruguay_2024.rds)          |


## Descripción del Dataframe

Las encuestas de cada país son filtradas para obtener información para el total del empleo urbano del país. El dataframe base_homog.Rds cuenta con las siguientes columnas:

- **PAIS**: País de la encuesta.
- **WEIGHT**: Ponderador.
- **SEXO**: Sexo.
- **EDAD**: Edad.
- **COND**: Condición de actividad.
- **CATOCUP**: Categoría ocupacional.
- **SECTOR**: Sector (Público, Privado sin servicio doméstico, Servicio Doméstico)
- **EDUC**: Máximo nivel educativo terminado (primary, intermediate, terciary)
- **PRECAPT**: Precariedad por trabajo part-time involuntario.
- **PRECAREG**: Precariedad por no registro de la relación laboral.
- **PRECATEMP**: Precariedad por trabajo temporario.
- **PRECASEG**: Precariedad por falta de aportes a la seguridad social.
- **TAMA**: Tamaño del establecimiento.
- **CALIF**: Calificación del puesto.
- **ING**: Ingreso de la ocupación principal en moneda local.
- **PPA**: Coeficiente de paridad de poder adquisitivo para el año de la encuesta.
- **ING_PPA**: Ingreso de la ocupación principal en moneda de paridad de poder adquisitivo del año correspondiente (USA = 1).
- **ANO**: Año de referencia de la encuesta.
- **PERIODO**: Período de referencia de la encuesta.
- **WEIGHT_W**: Ponderador de los ingresos de la ocupación principal (Sólo para Argentina).


¡Gracias por utilizar nuestro repositorio! Si tienes alguna pregunta o sugerencia, no dudes en contactarnos. También podés proponer modificaciones al código mediante un issue al repositorio. 

## Estructura del Repositorio

El repositorio está organizado de la siguiente manera:

- **base_homogenea.RDS**: Dataset principal con información homogénea de todos los países trabajados
- **Metadata.xlsx**: Documento que describe los criterios utilizados para la creación de variables de la base homogeneizada
- **\bases**: carpeta para almacenar bases de microdatos en el formato de publicación del instituto de estadística de cada país
- **\scripts**: scripts utilizados para transformar los datos por país de \bases en dataframes con datos homogéneos que se guardan en \bases_homog
- **\bases_homog**: carpeta para almacenar bases homogéneas por país
- **\genera_base_homogenea.R**: unifica toda la información procesada por país en un único dataframe
- **\Fuentes Complementarias**: archivos complementarios para procesar la información por país (clasificadores de ocupaciones, coeficientes de paridad de poder adquisitivo, entre otros)


# Repository precariedad.mundial

Welcome to the data repository for household survey processing, within the framework of the world precariousness research line of the Center for Studies on Population, Employment, and Development (CEPED - IIEEP - UBA). This project uses microdata from household surveys conducted by official statistical institutes in various countries around the world and converts them into a unified dataframe with standardized information.

## Repository Structure

The repository is organized as follows:

- **base_homogenea.RDS**: Main dataset with homogeneous information of all countries 
- **Metadata.xlsx**: Document describing the criteria used to create variables in the standardized database.
- **\Bases**: Folder to save microdata sets in the publication format of each country's statistical institute.
- **\scripts**: Includes country-specific scripts used to transform the data from each country into a homogeneous dataframe.
- **\bases_homog**: Folder to save homogeneous databases by country.
- **genera_base_homogenea.R**: unifies data from each country to generate the main dataset
- **\Fuentes Complementarias**: complementary files (Classifications of Occupations, purchasing power parity coefficients and others)


## Description of the Dataframe

Surveys from each country are filtered to obtain information for total urban employment in the country. The base_homog.Rds dataframe includes the following columns:

- **PAIS**: Country of the survey.
- **WEIGHT**: Weight.
- **SEXO**: Sex.
- **EDAD**: Age.
- **COND**: Activity condition.
- **CATOCUP**: Occupational category.
- **SECTOR**: Sector (Public, Private, Paid domestic workers)
- **EDUC**: Highest level of education completed (primary, secondary, tertiary)
- **PRECAPT**: Precariousness due to involuntary part-time work.
- **PRECAREG**: Precariousness due to lack of registration of the employment relationship.
- **PRECATEMP**: Precariousness due to temporary work.
- **PRECASEG**: Precariousness due to lack of social security contributions.
- **TAMA**: Establishment size.
- **CALIF**: Job qualification.
- **ING**: Principal occupation income in local currency.
- **ING_PPA**: Principal occupation income in purchasing power parity units of corresponding year (USA = 1).
- **ANO**: Survey reference year.
- **PERIODO**: Survey reference period.
- **WEIGHT_W**: Weights related to main job income (Only for Argentina).


Thank you for using our repository! If you have any questions or suggestions, please feel free to contact us. If you use information from this project, we appreciate citing this repository or one of our publications:

- [**La calidad del empleo en la Argentina reciente: un análisis sobre su relación con la calificación y el tamaño de las unidades productivas en perspectiva comparada** J Graña, G Weksler, F Lastra *Trabajo y Sociedad 38, 423-446*](https://www.unse.edu.ar/trabajoysociedad/38%20GRANA%20ET%20ALT%20La%20calidad%20del%20empleo%20en%20la%20Argentina.pdf)

- [**Calidad del empleo y estructura del mercado de trabajo en América Latina desde una perspectiva comparada** S Fernández-Franco, JM Graña, F Lastra, G Weksler *Ensayos de Economía 32 (61), 124-151*](https://doi.org/10.15446/ede.v32n61.100343)

