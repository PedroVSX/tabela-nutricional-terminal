(ns tabela-nutricional.core
  (:require [clj-http.client :as http]                      ;;para comunicação com APIs.
            [cheshire.core :as json]                        ;;para trabalhar com JSON.
            [clojure.string :as str])                       ;;para manipular strings.
  (:gen-class))

;;BUSCAR A LISTA DE ATIVIDADES
(defn buscar-dados-atividade []
  (try
    (let [resposta-busca (http/get "http://localhost:3000/atividades"
                                   {:headers {"Accept" "application/json"}})
          dados-atividade (json/parse-string (:body resposta-busca) true)] ; true => converte para keywords

      ;; Mostra os dados no terminal
      (println "Atividade:" (:atividade dados-atividade))
      (println "Tempo da Atividade:" (:tempo dados-atividade)))

    (catch Exception e
      (println "Nao foi posssivel encontrar os dados da atividade")
      (println "Por conta do erro:" (.getMessage e)))))

;; Função auxiliar com recursão de cauda para imprimir opções
(defn imprimir-opcoes-alimentos
  ([alimentos] (imprimir-opcoes-alimentos alimentos 0))
  ([alimentos idx]
   (when (seq alimentos)
     (let [atual (first alimentos)
           resto (rest alimentos)]
       (println (str idx ": " (:nome atual) " - " (:calorias atual) " kcal"))
       (recur resto (inc idx))))))

;; Função principal
(defn registrar-consumo-avancado []
  (println "===================== Registro de Consumo =====================\n")
  (print "Digite o nome do alimento: ") (flush)
  (let [alimento (read)]
    (print "Digite a porcao consumida em gramas: ") (flush)
    (let [porcao (read)
          url (str "http://localhost:3000/alimentos/" alimento)]
      (try
        (let [resposta (http/get url {:headers {"Accept" "application/json"}})
              body (json/parse-string (:body resposta) true)]

          (if (and body (seq body))
            (do
              (println "\n========================== Alimentos ==========================")
              (imprimir-opcoes-alimentos body)

              (print "\nEscolha o numero do alimento que deseja registrar: ") (flush)
              (let [indice (read)
                    escolhido (nth body indice)
                    dados-envio {:alimento (str (:nome escolhido))
                                 ;;:data data
                                 :caloria (:calorias escolhido)
                                 :quantidade porcao}]
                ;(println "Dados a enviar para o backend:" dados-envio)
                ;; Envia para a API (endpoint ainda será criado no back-end)
                (http/post "http://localhost:3000/consumo"
                           {:body (json/encode dados-envio)
                            :headers {"Content-Type" "application/json"}})

                (println "\n===============================================================\n")
                (println "Alimento registrado com sucesso!")
                (println "Alimento:" (:nome escolhido))
                (println "Caloria:" (:calorias escolhido) "kcal")
                (println "Porcao consumida:" porcao "g")))

            (println "Nenhum alimento encontrado.")))

        (catch Exception e
          (println "Erro ao registrar alimento:" (.getMessage e))))))
  (println "\n===============================================================\n")
  )

;; Função auxiliar com recursão de cauda para imprimir opções
(defn imprimir-opcoes-atividades
  ([atividades] (imprimir-opcoes-atividades atividades 0))
  ([atividades idx]
   (when (seq atividades)
     (let [atual (first atividades)
           resto (rest atividades)]
       (println (str idx " - " (:atividade atual) " (" (:calorias atual) " kcal)"))
       (recur resto (inc idx))))))

;; Função principal
(defn registrar-atividade-avancado []
  (print "Digite o nome da atividade: ") (flush)
  (let [atividade (read)]
    (print "Digite a duracao da atividade em minutos: ") (flush)
    (let [tempo (read)
          url (str "http://localhost:3000/exercicios/" atividade "/" tempo)]
      (try
        (let [resposta (http/get url {:headers {"Accept" "application/json"}})
              body (json/parse-string (:body resposta) true)
              opcoes (:resultados body)]

          (if (and opcoes (seq opcoes))
            (do
              (println "\n==================== Atividades Sugeridas =====================")
              (imprimir-opcoes-atividades opcoes)

              (print "\nEscolha o numero da atividade que deseja registrar: ") (flush)
              (let [indice  (read)
                    escolhida (nth opcoes indice)
                    dados-envio {:atividade (:atividade escolhida)
                                 :tempo tempo}]
                ;; Envia para a API
                (http/post "http://localhost:3000/atividade"
                           {:body (json/encode dados-envio)
                            :headers {"Content-Type" "application/json"}})
                (println "\n===============================================================\n")
                (println "Atividade registrada com sucesso!")
                (println "Atividade:" (:atividade escolhida))
                (println "Calorias gastas:" (:calorias escolhida) "kcal"))
                (println "\n===============================================================\n"))

            (println "Nenhuma atividade encontrada.")))

        (catch Exception e
          (println "Erro ao registrar atividade:" (.getMessage e)))))))


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

         (println "\nCadastro concluido com sucesso!")

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

(defn consultar-extrato []
  (println "Consultar Extrato")
  (println "\n======================= Dados Pessoais ========================")
  (buscar-dados-atividade)

  )

(defn saldo-calorias []
  (println "Seu saldo de calorias é: SALDO DE CALORIAS"))

(defn menu-cliente-cadastrado []
  (println "====================== Menu de Operacoes ======================")
  (println "1 - Consultar dados pessoais (nome, altura, peso, idade e sexo).")
  (println "2 - Registrar consumo de alimento (ganho de caloria).")
  (println "3 - Registrar realizacao de atividade fisica (perda de caloria).")
  (println "4 - Consultar extrato de transacoes (por periodo).")
  (println "5 - Consultar saldo de calorias (por periodo).")
  (println "6 - Voltar.")
  (print "Escolha uma opcao: ") (flush)
  (let [opcao (read)]
  (println "===============================================================\n")
    (cond
      (= opcao 1) (do (consultar-dados) (recur))
      (= opcao 2) (do (registrar-consumo-avancado) (recur))
      (= opcao 3) (do (registrar-atividade-avancado) (recur))
      (= opcao 4) (do (consultar-extrato) (recur))
      (= opcao 5) (do (saldo-calorias) (recur))
      (= opcao 6) (println "Voltando ao menu principal...\n")
      :else (do (println "Opcao invalida.") (recur)))))

(defn cadastrar-usuario []
  (println "\n=================== Informe os dados abaixo ===================")
  (print "Digite seu nome: ") (flush)
  (let [nome (read)]
    (print "Digite sua altura (em cm): ") (flush)
    (let [altura (read)]
      (print "Digite seu peso (em kg): ") (flush)
      (let [peso (read)]
        (print "Digite sua idade (em anos): ") (flush)
        (let [idade (read)]
          (print "Diga seu sexo (Masculino/Feminino): ") (flush)
          (let [sexo (read)]
            (salvar-dados-usuario nome altura peso idade sexo))))))
  (println "===============================================================\n")
  (menu-cliente-cadastrado)
  )

(defn menu-cliente-nao-cadastrado []
  (println "\n======================= Menu de Cadastro ======================")
  (println "1 - Registrar novo usuario")
  (println "2 - Voltar")
  (print "Escolha uma opcao: ")
  (flush)
  (let [opcao (read)]
    (println "===============================================================")
    (cond
      (= opcao 1) (cadastrar-usuario)
      (= opcao 2) (println "Voltando ao menu principal...")
      :else (do (println "Opcao invalida.") (recur)))))

(defn menu-inicial [opcao]
  (cond
    (= opcao 1) (menu-cliente-nao-cadastrado)
    :else (println "\nOpcao invalida... Tente novamente!")))

(defn menu-cliente []
  (println "===============================================================")
  (print (str
           "Seja bem vindo a nossa Calculadora\n"

           "  1 - Quero me cadastrar.\n"
           "  2 - Sair\n"
           "  Escolha uma opcao: ")) (flush)
  )

(defn menu-recursivo []
  (menu-cliente)
  (let [opcao (read)]
    (if (= opcao 2)
      (print (str
               "\n===============================================================\n
                 Agradecemos pela sua visita!\n
===============================================================\n"))
      (do
        (menu-inicial opcao)
        (recur)))))

(defn -main []
  (menu-recursivo))
