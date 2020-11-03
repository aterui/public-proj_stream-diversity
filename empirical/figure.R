ggplot(dat, aes(x = p_branch, y = gamma, color = region)) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_point() +
  geom_smooth(method = "lm", se = F)

ggplot(dat, aes(x = area, y = gamma, color = region)) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_point() +
  geom_smooth(method = "lm", se = F)

ggplot(dat, aes(x = p_branch, y = mu_alpha, color = region)) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_point() +
  geom_smooth(method = "lm", se = F)

ggplot(dat, aes(x = p_branch, y = beta, color = region)) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_point() +
  geom_smooth(method = "lm", se = F)

