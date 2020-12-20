library(ggfortify)

csv <- dt %>%
    subtract(1) %>%
    as.character() %>%
    str_replace_all("(.+)-(.+)-(.+)", "\\2-\\3-\\1.csv")

df <- glue("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/{csv}") %>%
    read_csv() %>%
    dplyr::filter(
        ISO3 == "USA",
        !is.na(Lat)
    )

# Presidential Results
nyt <- "https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/results.json" %>%
    fromJSON() %>%
    .[["data"]] %>%
    .[["races"]] %>%
    as_tibble() %>%
    dplyr::select(
        State = state_name,
        Party = leader_party_id
    ) %>%
    mutate(Party = str_to_title(Party))

# Governors
gov <- "https://en.wikipedia.org/wiki/List_of_current_United_States_governors" %>%
    read_html() %>%
    html_nodes("body") %>%
    xml_find_all("//table[contains(@class, 'sortable wikitable')]") %>%
    .[[1]] %>%
    html_table(header = FALSE, fill = TRUE) %>%
    dplyr::select(c(1, 3, 5)) %>%
    as_tibble() %>%
    dplyr::slice(-1, -2) %>%
    setNames(c("State", "Governor", "Party")) %>%
    mutate(
        Party = str_extract(Party, "Republican|Democratic")
    ) %>%
    bind_rows(
        tibble(
            State = "District of Columbia",
            Governor = NA_character_,
            Party = "Democratic"
        )
    )

# Population Density
dens <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population_density" %>%
    read_html() %>%
    html_nodes("body") %>%
    xml_find_all("//table[contains(@class, 'wikitable sortable')]") %>%
    .[[1]] %>%
    html_table(header = FALSE, fill = TRUE) %>%
    as_tibble() %>%
    setNames(
        vapply(.[1:2,], function(x){
            paste(unique(x), collapse = " ")
        },
        character(1)
        )
    ) %>% 
    dplyr::slice(-(1:2)) %>%
    dplyr::select(
        State = `State etc.`,
        Population = `Population Numbers`,
        Area = `Land area km2`
    ) %>%
    mutate(
        Population = str_remove_all(Population, ",") %>% as.numeric(),
        Area = str_remove_all(Area, ",|\\.") %>% as.numeric(),
        Density = Population / Area
    )

df %>% 
    mutate(
        pop = (Incident_Rate *1e-5 / Confirmed)^(-1), 
        Death_Rate = round(pop / Deaths, 0)
    ) %>% 
    dplyr::select(pop, Death_Rate, everything()) %>%
    arrange(Death_Rate) %>%
    # left_join(nyt, by = c("Province_State" = "State")) %>%
    left_join(gov, by = c("Province_State" = "State")) %>%
    mutate(
        Party = str_extract(Party, "Democrat|Republican"),
        State = fct_inorder(Province_State)
    ) %>% 
    ggplot(aes(State, Death_Rate)) + 
    geom_col(aes(fill = Party), colour = "black", size = 1/4) + 
    geom_text(aes(label = Death_Rate, colour = Party), nudge_y = 250) +
    geom_hline(yintercept = 1000, colour = "black", linetype = 2) + 
    coord_flip() + 
    scale_y_continuous(expand = expansion(c(0, 0.05)), breaks = seq(1, 10)*1e3) + 
    scale_fill_manual(values = c(Democrat = "blue", Republican = "red")) +
    scale_colour_manual(values = c(Democrat = "blue", Republican = "red")) +
    labs(y = "One death every 'x' people")


df %>% 
    mutate(
        pop = (Incident_Rate *1e-5 / Confirmed)^(-1), 
        Death_Rate = round(pop / Deaths, 0),
        DPM = 1e6/ Death_Rate
    ) %>% 
    dplyr::select(pop, Death_Rate, everything()) %>%
    arrange(Death_Rate) %>%
    # left_join(nyt, by = c("Province_State" = "State")) %>%
    left_join(gov, by = c("Province_State" = "State")) %>%
    left_join(dens, by = c("Province_State" = "State")) %>%
    mutate(
        Party = str_extract(Party, "Democrat|Republican"),
        State = fct_inorder(Province_State)
    ) %>%
    ggplot(aes(Density, DPM, colour = Party)) +
    geom_point() +
    geom_text_repel(aes(label = State), show.legend = FALSE) +
    geom_smooth(method = "lm") +
    scale_colour_manual(values = c(Democrat = "blue", Republican = "red")) +
    scale_x_log10(labels = scales::comma) +
    scale_y_log10(labels = scales::comma, breaks = c(1, 2, 4, 10, 20)*100, expand = expansion(c(0.05, 0.005))) +
    labs(
        x = "Population / km2",
        y = "Deaths / Million"
    )


df %>% 
    mutate(
        pop = (Incident_Rate *1e-5 / Confirmed)^(-1), 
        Death_Rate = round(pop / Deaths, 0),
        DPM = 1e6/ Death_Rate
    ) %>% 
    dplyr::select(pop, Death_Rate, everything()) %>%
    arrange(Death_Rate) %>%
    left_join(nyt, by = c("Province_State" = "State")) %>%
    left_join(dens, by = c("Province_State" = "State")) %>%
    mutate(
        Party = str_extract(Party, "Democrat|Republican"),
        State = fct_inorder(Province_State),
        Dem = str_detect(Party, "Dem")
    ) %>%
    with(
        glm(Dem ~ log10(Density) , family = "binomial")
    ) %>%
    summary()

