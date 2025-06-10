(ns tabela-nutricional.core
  (:require [clj-http.client :as http]                      ;;para comunicação com APIs.
            [cheshire.core :as json]                        ;;para trabalhar com JSON.
            [clojure.string :as str])                       ;;para manipular strings.
  (:gen-class))

;; FUNÇÕES AUXILIARES

(defn arrendondamento [x]
  (/ (Math/round (* x 10.0)) 10.0)) ;; Math/round -> arredondar um número decimal para o inteiro mais próximo.

(defn caloria-alimento-real [porcao calorias]
  (arrendondamento (/ (* (double porcao) (double calorias)) 100.0)))

(defn quilo-para-libra [peso]
  (Math/round (* peso 2.20462262)))

(defn calcular-total [chave-da-caloria extrato]
  (let [total (reduce
                (fn [acc item]
                  (+ acc (get item chave-da-caloria)))
                0
                extrato)]
    (arrendondamento total))) ; Aplica o arredondamento ao resultado final

(defn usuario-existe? []
  (try
    (let [resposta (http/get "http://localhost:3000/usuarios/1"
                             {:headers {"Accept" "application/json"}})
          dados (json/parse-string (:body resposta) true)]
      (boolean (:nome dados))) ;; Verifica se o nome está presente, o que indica um cadastro válido
    (catch Exception e
      false))) ;; Se der qualquer erro, assume que o usuário não existe

;; FIM FUNÇÕES AUXILIARES

;; INICIO USUARIO
(defn salvar-dados-usuario [nome altura peso idade sexo]
  (let [dados-usuario {:nome   nome
                       :altura altura
                       :peso   peso
                       :idade  idade
                       :sexo   sexo}]
    ;;(println (str "\n Dados do usuario salvos: " dados-usuario "\n"))

    (try (http/post "http://localhost:3000/usuarios"
                    {:body    (json/encode dados-usuario)
                     :headers {"Content-Type" "application/json"}})
         (println "\n===============================================================")
         (println "\nCadastro concluido com sucesso!\n")

         (catch Exception e
           (println "Nao foi possivel concluir seu registro de consumo, pois" (.getMessage e))))))

(defn buscar-dados-usuario []
  (try
    (let [resposta-busca (http/get "http://localhost:3000/usuarios/1"
                                   {:headers {"Accept" "application/json"}})
          dados-usuario (json/parse-string (:body resposta-busca) true)] ; true => converte para keywords

      ;; Mostra os dados no terminal
      (println "Nome:" (:nome dados-usuario))
      (println "Altura:" (:altura dados-usuario) "cm")
      (println "Peso:" (:peso dados-usuario) "kg")
      (println "Idade:" (:idade dados-usuario) "anos")
      (println "Sexo:" (:sexo dados-usuario)))

    (catch Exception e
      (println "Nao foi posssivel encontrar os dados do usuario")
      (println "Por conta do erro:" (.getMessage e)))))

(defn consultar-dados []
  (println "======================= Dados Pessoais ========================")
  (buscar-dados-usuario)

  (print "\nDigite 0 para retornar ao menu anterior: ") (flush)
  (let [opcao (read)]
    (println "===============================================================")
    (cond
      (= opcao 0) (println "Voltando ao menu principal...\n")
      :else (do (println "Opcao invalida...\n") (recur)))))

;; FIM USUARIO


;; INICIO ALIMENTO

(defn imprimir-opcoes-alimentos
  ([alimentos] (imprimir-opcoes-alimentos alimentos 0))
  ([alimentos idx]
   (when (seq alimentos)                                  ;; Usa when com seq para verificar se a lista ainda tem itens.
     (let [atual (first alimentos)
           resto (rest alimentos)]
       (println (str idx ": " (:nome atual)))
       (recur resto (inc idx))))))

(defn registrar-consumo-avancado []
  (println "===================== Registro de Consumo =====================\n")
  (print "Digite o nome do alimento: ") (flush)
  (let [alimento (read)]
    (print "Digite a porcao consumida em gramas: ") (flush)
    (let [porcao (read)
          url (str "http://localhost:3000/alimentos/" alimento)]

      ;; Leitura da data como string
      (read-line) ;; descarta o \n deixado por 'read' anterior
      (print "Digite a data da refeicao (dd/mm/aaaa): ") (flush)
      (let [data (read-line)]

        (try
          (let [resposta (http/get url {:headers {"Accept" "application/json"}})
                body (json/parse-string (:body resposta) true)]

            (if (and body (seq body))
              (do
                (println "\n========================== Alimentos ==========================")
                (imprimir-opcoes-alimentos body)

                (print "\nEscolha o numero do alimento que deseja registrar: ") (flush)
                (let [indice (read)
                      escolhido (nth body indice)           ;;nth -> pega o item correspondente no vetor body
                      caloria-real (caloria-alimento-real porcao (:calorias escolhido))
                      dados-envio {:alimento (str (:nome escolhido))
                                   :data data
                                   :caloria caloria-real
                                   :quantidade porcao}]

                  (http/post "http://localhost:3000/consumo"
                             {:body (json/encode dados-envio)
                              :headers {"Content-Type" "application/json"}})

                  (println "\n===============================================================")
                  (println "Alimento registrado com sucesso!")
                  (println "Alimento:" (:nome escolhido))
                  (println "Data:" data)
                  (println "Caloria:" caloria-real "kcal")
                  (println "Porcao consumida:" porcao"g")))

              (println "Nenhum alimento encontrado.")))

          (catch Exception e
            (println "Erro ao registrar alimento:" (.getMessage e)))))))
  (println "===============================================================\n"))

;; FIM ALIMENTO


;; INICIO ATIVIDADES

(defn imprimir-opcoes-atividades
  ([atividades] (imprimir-opcoes-atividades atividades 0))
  ([atividades idx]
   (when (seq atividades)
     (let [atual (first atividades)
           resto (rest atividades)]
       (println (str idx " - " (:atividade atual)))
       (recur resto (inc idx))))))

(defn registrar-atividade-avancado []
  ;; Solicita nome da atividade
  (print "Digite o nome da atividade: ") (flush)
  (let [atividade (read)]

    ;; Solicita tempo
    (print "Digite a duracao da atividade em minutos: ") (flush)
    (let [tempo (read)

          ;; Busca dados do usuário (peso em kg)
          resposta-busca (http/get "http://localhost:3000/usuarios/1"
                                   {:headers {"Accept" "application/json"}})
          dados-usuario (json/parse-string (:body resposta-busca) true)
          peso-kg (:peso dados-usuario)
          peso-lb (quilo-para-libra peso-kg) ; Convertendo para libras

          ;; Monta URL com peso convertido
          url (str "http://localhost:3000/exercicios/" atividade "/" tempo "/" peso-lb)]

      ;;(println "\nConsultando: " url)

      (read-line) ;; limpa o buffer do read anterior
      ;; Solicita data
      (print "Digite a data da atividade (dd/mm/aaaa): ") (flush)
      (let [data (read-line)]
        (try
          ;; Chamada para API de exercícios
          (let [resposta (http/get url {:headers {"Accept" "application/json"}})
                body (json/parse-string (:body resposta) true)
                opcoes (:resultados body)]

            ;; Verifica se há resultados
            (if (and opcoes (seq opcoes))
              (do
                (println "\n==================== Atividades Sugeridas =====================")
                (imprimir-opcoes-atividades opcoes)

                ;; Escolha de atividade
                (print "\nEscolha o numero da atividade que deseja registrar: ") (flush)
                (let [indice (read)
                      escolhida (nth opcoes indice)
                      dados-envio {:atividade (:atividade escolhida)
                                   :tempo tempo
                                   :data data
                                   :calorias (:calorias escolhida)}]

                  ;; Envio da atividade
                  (http/post "http://localhost:3000/atividade"
                             {:body (json/encode dados-envio)
                              :headers {"Content-Type" "application/json"}})


                  ;; Mensagem de sucesso
                  (println "\n===============================================================")
                  (println "Atividade registrada com sucesso!")
                  (println "Atividade:" (:atividade escolhida))
                  (println "Calorias gastas:" (:calorias escolhida) "kcal")
                  (println "Data:" data)
                  (println "===============================================================\n")))

              ;; Nenhum resultado
              (println "Nenhuma atividade encontrada.\n")))

          ;; Tratamento de erro
          (catch Exception e
            (println "Erro ao registrar atividade:" (.getMessage e))))))))

;; FIM ATIVIDADES


;; INICIO EXTRATO

(defn buscar-extrato-alimento [data-inicial data-final]
  (let [payload {:data-inicial data-inicial
                 :data-final data-final}]
    (let [resposta (http/post "http://localhost:3000/extrato/alimento"
                              {:body (json/generate-string payload)
                               :headers {"Content-Type" "application/json"
                                         "Accept" "application/json"}})
          dados (json/parse-string (:body resposta) true)]
      dados)))

(defn buscar-extrato-atividade [data-inicial data-final]
  (let [payload {:data-inicial data-inicial
                 :data-final data-final}]
    (let [resposta (http/post "http://localhost:3000/extrato/atividade"
                              {:body (json/generate-string payload)
                               :headers {"Content-Type" "application/json"
                                         "Accept" "application/json"}})
          dados (json/parse-string (:body resposta) true)]
      dados)))

(defn consultar-extrato-alimento []
  (println "===================== Consultar Extrato =======================")

  (read-line) ;; descarta o \n deixado por 'read' anterior
  (print "Digite a data inicial do extrato (dd/mm/aaaa): ") (flush)
  (let [data-inicial (read-line)]
    (print "Digite a data final do extrato (dd/mm/aaaa): ") (flush)
    (let [data-final (read-line)]
      (try
        ;; Extrato de alimentos
        (let [extrato-alimento (buscar-extrato-alimento data-inicial data-final)]

          (println "\n======================== Alimentos ==========================")
          (println (str "========= Registrados em (" data-inicial ") - (" data-final ") ========\n"))

          (if (seq extrato-alimento)
            (mapv (fn [item]
                    (println "Alimento:" (:alimento item))
                    (println "Calorias:" (:caloria item) "kcal")
                    (println "Quantidade:" (:quantidade item) "g")
                    (println "Data de Consumo:" (:data-consumo item))
                    (println "-------------------------------------------------------------"))
                  extrato-alimento)
            (println "Nenhum alimento registrado no periodo informado.")))

        ;; Extrato de atividades
        (let [extrato-atividade (buscar-extrato-atividade data-inicial data-final)]
          (println "\n======================= Atividades ==========================")
          (println (str "========= Registrados em (" data-inicial ") - (" data-final ") ========\n"))
          (if (seq extrato-atividade)
            (mapv (fn [item]
                    (println "Atividade:" (:atividade item))
                    (println "Calorias:" (:calorias item) "calorias gastas")
                    (println "Tempo:" (:tempo item) "minutos")
                    (println "Data:" (:data item))
                    (println "-------------------------------------------------------------"))
                  extrato-atividade)
            (println "Nenhuma atividade registrada no periodo informado.")))

        (catch Exception e
          (println "Erro ao consultar extrato:" (.getMessage e)))))

    (println "\n===============================================================\n")))

;; FIM EXTRATO


;; INICIO SALDO

(defn saldo-calorias []
  (println "===================== Consultar Saldo =======================")
  (read-line)
  (print "Digite a data inicial do extrato (dd/mm/yyyy): ") (flush)
  (let [data-inicial (read-line)]
    (print "Digite a data final do extrato (dd/mm/yyyy): ") (flush)
    (let [data-final (read-line)]
      (try
        (let [extrato-alimento (buscar-extrato-alimento data-inicial data-final)
                calorias-consumidas (calcular-total :caloria extrato-alimento)

              extrato-atividade (buscar-extrato-atividade data-inicial data-final)
                calorias-gastas (calcular-total :calorias extrato-atividade)

              saldo (- calorias-consumidas calorias-gastas)]

          (println "\n=================== Saldo de Calorias =======================")
          (println (str "============ Resumo de (" data-inicial ") - (" data-final ") ==========\n"))

          (println "Calorias consumidas: " calorias-consumidas "kcal")
          (println "Calorias gastas:     " calorias-gastas "kcal")
          (println "Saldo calorico:      " saldo "kcal")
          saldo
          (println "\n===============================================================\n"))

        (catch Exception e
          (println "Erro ao consultar extrato:" (.getMessage e))
          nil)))))

;; FIM SALDO

(defn menu-cliente-cadastrado []
  (println "====================== Menu de Operacoes ======================")
  (println "1 - Consultar dados pessoais (nome, altura, peso, idade e sexo).")
  (println "2 - Registrar consumo de alimento (ganho de caloria).")
  (println "3 - Registrar realizacao de atividade fisica (perda de caloria).")
  (println "4 - Consultar extrato de transacoes (por periodo).")
  (println "5 - Consultar saldo de calorias (por periodo).")
  (println "6 - Sair.")
  (print "Escolha uma opcao: ") (flush)
  (let [opcao (read)]
    (println "===============================================================\n")
    (cond
      (= opcao 1) (do (consultar-dados) (recur))
      (= opcao 2) (do (registrar-consumo-avancado) (recur))
      (= opcao 3) (do (registrar-atividade-avancado) (recur))
      (= opcao 4) (do (consultar-extrato-alimento) (recur))
      (= opcao 5) (do (saldo-calorias) (recur))
      (= opcao 6) (println
                    "                  Agradecemos pela sua visita!\n
===============================================================\n")
      :else (do (println "Opcao invalida.") (recur)))))

(defn cadastrar-usuario []
  (println "\n=================== Informe os dados abaixo ===================")
  (read-line) ;; <- Consome o \n pendente, evita pular a leitura do nome
  (print "Digite seu primeiro nome: ") (flush)
  (let [nome (read-line)]
    (print "Digite sua altura (em cm): ") (flush)
    (let [altura (Double/parseDouble (read-line))]
      (print "Digite seu peso (em kg): ") (flush)           ;; maior que 23 e menor que 225
      (let [peso (Double/parseDouble (read-line))]
        (print "Digite sua idade (em anos): ") (flush)
        (let [idade (Integer/parseInt (read-line))]
          (print "Diga seu sexo (Masculino/Feminino): ") (flush)
          (let [sexo (read-line)]
            (salvar-dados-usuario nome altura peso idade sexo))))))
  (println "===============================================================\n")
  (menu-cliente-cadastrado)
  )

(defn menu-cliente-nao-cadastrado []
  (println "============= Seja bem-vindo a nossa calculadora! =============")
  (println "1 - Quero me cadastrar.")
  (println "2 - Sair.")
  (print "Escolha uma opcao: ") (flush)

  (let [opcao (read)]
    (println "===============================================================")
    (cond
      (= opcao 1) (do
                    (cadastrar-usuario))
      (= opcao 2) (do
                    (println
                      "\n===============================================================\n
                       Agradecemos pela sua visita!\n
===============================================================\n")
                    (System/exit 0)) ; encerra o programa
      :else (do (println "Opção invalida.") (recur)))))

(defn menu-recursivo []
  (if (usuario-existe?)
    (menu-cliente-cadastrado)
    (menu-cliente-nao-cadastrado)))

(defn -main []
  (menu-recursivo))