

accid_death_age <- read_csv("http://datos.cali.gov.co/dataset/08e53d46-a82a-43d3-8b9c-2fb35edc56e1/resource/a6480c19-f1ac-4c8f-9ed6-a958ccb9774d/download/muertes_accidentales_seg_n_rango_de_edades_en_el_municipio_de_santiago_de_cali__2004_-_2017-1.csv")

accid_death_age <- accid_death_age[-14,]

accid_death_age <- accid_death_age %>% rename(Age = "RANGO_EDAD")

accid_death_age$Age[13] <- "60+"

accid_death_age[,2:15] <- sapply(accid_death_age[,2:15], as.numeric)

accid_death_age_long <- accid_death_age %>% 
	pivot_longer(cols = "2004":"2017", 
							 names_to = "Year",
							 values_to = "freq")

a <- accid_death_age_long %>% ggplot(aes(x= Age, y = freq)) +
		geom_col(aes(fill = Year), position = position_dodge()) +
		facet_wrap(~Year, nrow = 2) +
		theme_bw() +
		labs(title = "Accidental deaths by age",
				 subtitle = "Cali (Colombia), 2004-2017"
				 , caption = "Source: http://datos.cali.gov.co/") +
		ylab("Frequency") +
		xlab("Time of day") +
		# scale_colour_viridis_d() +
		scale_fill_viridis_d() +
		theme(plot.subtitle = element_text(face = "italic"), 
					axis.text.x = element_text(angle = 90, vjust = 0.5),
					legend.position = "none",
					axis.title.x = element_blank(),
					text = element_text(size = 10))

b <- accid_death_age_long %>% ggplot(aes(x= Age, y = freq)) +
	geom_point(aes(colour = Year), position = position_dodge()) +
	geom_line(aes(group = as.factor(Year), colour = as.factor(Year)), alpha = 0.1) +
	geom_boxplot(colour = "gray", fill = "gray", alpha = 0.4, notch = TRUE) +
	# facet_wrap(~year) +
	theme_bw() +
	labs(title = "Accidental deaths by age",
			 subtitle = "Cali (Colombia), 2004-2017"
			 , caption = "Source: http://datos.cali.gov.co/") +
	ylab("Frequency") +
	xlab("Time of day") +
	scale_colour_viridis_d() +
	# scale_fill_viridis_d() +
	theme(plot.subtitle = element_text(face = "italic"), 
				axis.text.x = element_text(angle = 0), 
				legend.position = "bottom",
				axis.title.x = element_blank(),
				text = element_text(size = 12))

bp <- ggplotly(b) %>%
	layout(title = list(text = paste0('Accidental deaths by age',
																		'<br>',
																		'<sup>',
																		'Cali (Colombia), 2004-2017 | Source: http://datos.cali.gov.co/',
																		'</sup>')),
				 legend =list(orientation = "h", y = -0.1))

subplot(bp,a) %>%
	layout(title = list(text = paste0('Accidental deaths by age',
																		'<br>',
																		'<sup>',
																		'Cali (Colombia), 2004-2017 | Source: http://datos.cali.gov.co/',
																		'</sup>')),
				 legend =list(orientation = "h", y = -0.1))