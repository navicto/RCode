base =2
flu_t=8
flu_f=2
flu_total = flu_f + flu_t
flu_t_cough_t = 4
flu_f_cough_t = 1
flu_t_cough_f = 4
flu_f_cough_f = 1
cough_t = flu_t_cough_t + flu_f_cough_t
cough_f = flu_t_cough_f + flu_f_cough_f
cough_total = cough_t + cough_f

h_flu=-(flu_t/flu_total)*log(flu_t/flu_total,base) - (flu_f/flu_total)*log(flu_f/flu_total,base)
h_cough_t = -(flu_t_cough_t/cough_t)*log(flu_t_cough_t/cough_t,base) - (flu_f_cough_t/cough_t)*log(flu_f_cough_t/cough_t,base)
h_cough_f = -(flu_t_cough_f/cough_f)*log(flu_t_cough_f/cough_f,base) - (flu_f_cough_f/cough_f)*log(flu_f_cough_f/cough_f,base)
h_cough = (cough_t/cough_total)*h_cough_t + (cough_f/cough_total)*h_cough_f
h_flu - h_cough
