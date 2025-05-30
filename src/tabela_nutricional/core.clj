(ns tabela-nutricional.core
  (:require [clj-http.client :as http]                      ;;para comunicação com APIs.
            [cheshire.core :as json]                        ;;para trabalhar com JSON.
            [clojure.string :as str])                       ;;para manipular strings.
  (:gen-class))

;;Responsável por "enviar" os dados/requisições

(defn salvar-dados-usuario [nome altura peso idade sexo]
  (let [dados-usuario {:nome nome
               :altura altura
               :peso peso
               :idade idade
               :sexo sexo}]

      (println (str "\n Dados do usuario salvos: " dados-usuario "\n"))

      (try (http/post "http://localhost:3000/usuarios"
                   {:body (json/encode dados-usuario)
                    :headers {"Content-Type" "application/json"}})

        (println "Cadastro concluido com sucesso!")

        (catch Exception e
          (println "Nao foi possivel concluir seu registro de consumo, pois" (.getMessage e))))))

(defn salvar-dados-consumo [alimento quantidade]
  (let [dados-consumo {:alimento alimento
               :quantidade quantidade}]
    (println (str "\n Dados do consumo salvos: " dados-consumo "\n"))

    (try (http/post "http://localhost:3000/COLOCAR O CAMINHO PARA SALVAR OS DADOS DO USUARIO"
                    {:body (json/encode dados-consumo)
                     :headers {"Content-Type" "application/json"}})

         (println "Consumo registrado com sucesso!")

         (catch Exception e
           (println "Nao foi possivel concluir seu registro de consumo, pois" (.getMessage e))))))

(defn salvar-dados-atividade [atividade tempo]
  (let [dados-atividade {:atividade atividade
               :tempo tempo}]
    (println (str "\n Dados da atividade salvos: " dados-atividade "\n"))

    (try (http/post "http://localhost:3000/COLOCAR O CAMINHO PARA SALVAR OS DADOS DO USUARIO"
                    {:body (json/encode dados-atividade)
                     :headers {"Content-Type" "application/json"}})

         (println "Registro de atividade feito com sucesso!")
         (catch Exception e
           (println "Nao foi possivel concluir seu registro de consumo, pois" (.getMessage e))))))

;;Responsável por "receber" os dados/requisições

;;FEITO
(defn buscar-dados-usuario []
  (try
    (let [resposta-busca (http/get "http://localhost:3000/usuarios/1"
                             {:headers {"Accept" "application/json"}})
          dados-usuario (json/parse-string (:body resposta-busca) true)] ; true => converte para keywords

      ;; Mostra os dados no terminal
      (println "Nome:" (:nome dados-usuario))
      (println "Altura:" (:altura dados-usuario))
      (println "Peso:" (:peso dados-usuario))
      (println "Idade:" (:idade dados-usuario))
      (println "Sexo:" (:sexo dados-usuario)))

    (catch Exception e
      (println "Erro ao buscar dados do usuário:" (.getMessage e)))))

(defn buscar-dados-consumo []
  (try
    (let [resposta-busca (http/get "http://localhost:3000/CAMINHO PARA BUSCAR DADOS"
                                   {:headers {"Accept" "application/json"}})
          dados-consumo (json/parse-string (:body resposta-busca) true)] ; true => converte para keywords
      ;; Mostra os dados no terminal
      (println "\n--- Dados do Consumo ---")
      (println "Alimento:" (:alimento dados-consumo))
      (println "Quantidade:" (:quantidade dados-consumo)))

    (catch Exception e
      (println "Erro ao buscar dados de consumo:" (.getMessage e)))))

(defn buscar-dados-atividade []
  (try
    (let [resposta-busca (http/get "http://localhost:3000/CAMINHO PARA BUSCAR DADOS"
                                   {:headers {"Accept" "application/json"}})
          dados-atividade (json/parse-string (:body resposta-busca) true)] ; true => converte para keywords
      ;; Mostra os dados no terminal
      (println "\n--- Dados do usuário ---")
      (println "Atividade:" (:atividade dados-atividade))
      (println "Tempo:" (:tempo dados-atividade)))

    (catch Exception e
      (println "Erro ao buscar dados de atividade:" (.getMessage e)))))

(defn menu-cliente []
  (print (str
           "Seja bem vindo a nossa Calculadora\n"
           "Ja e nosso cliente?\n"

           "  1 - Sim, sou cliente.\n"
           "  2 - Nao, quero me cadastrar.\n"
           "  3 - Sair\n"
           "  Escolha uma opcao: ")) (flush)
  )

(defn consultar-dados []
  (println "\n============= Dados Pessoais =============")
  (buscar-dados-usuario)

  (print "Digite 0 para retornar ao menu anterior: ") (flush)
  (let [opcao (read)]
    (cond
      (= opcao 0) (println "Voltando ao menu principal...")
      :else (do (println "Opcao invalida.") (recur)))))

(defn registrar-consumo []
  (print "Digite o nome do alimento: ") (flush)
  (let [alimento (read)]
    (print "Digite a quantidade: ") (flush)
    (let [quantidade (read)]
      (salvar-dados-consumo alimento quantidade))))

(defn registrar-atividade []
  (print "Digite o nome da atividade: ") (flush)
  (let [atividade (read)]
    (print "Digite quanto tempo voce praticou: ") (flush)
    (let [tempo (read)]
      (salvar-dados-consumo atividade tempo))))

(defn consultar-extrato []
  (println "Consultar Extrato"))

(defn saldo-calorias []
  (println "Seu saldo de calorias é: SALDO DE CALORIAS"))

(defn menu-cliente-cadastrado []
  (println "\n--- Menu Cliente Cadastrado ---")
  (println "1 - Consultar dados pessoais (altura, peso, idade e sexo).")
  (println "2 - Registrar consumo de alimento (ganho de caloria).")
  (println "3 - Registrar realizacao de atividade fisica (perda de caloria).")
  (println "4 - Consultar extrato de transacoes (por periodo).")
  (println "5 - Consultar saldo de calorias (por periodo).")
  (println "6 - Voltar.")
  (print "Escolha uma opcao: ") (flush)
  (let [opcao (read)]
    (cond
      (= opcao 1) (do (consultar-dados) (recur))
      (= opcao 2) (do (registrar-consumo) (recur))
      (= opcao 3) (do (registrar-atividade) (recur))
      (= opcao 4) (do (consultar-extrato) (recur))
      (= opcao 5) (do (saldo-calorias) (recur))
      (= opcao 6) (println "Voltando ao menu principal...")
      :else (do (println "Opcao invalida.") (recur)))))

(defn cadastrar-usuario []
  (print "Digite seu nome: ") (flush)
  (let [nome (read)]
    (print "Digite sua altura: ") (flush)
    (let [altura (read)]
      (print "Digite seu peso: ") (flush)
      (let [peso (read)]
        (print "Digite sua idade: ") (flush)
        (let [idade (read)]
          (print "Diga seu sexo: ") (flush)
          (let [sexo (read)]
            (salvar-dados-usuario nome altura peso idade sexo)))))))

(defn menu-cliente-nao-cadastrado []
  (println "\n--- Menu Cadastro ---")
  (println "1 - Registrar novo usuario")
  (println "2 - Voltar")
  (print "Escolha uma opcao: ")
  (flush)
  (let [opcao (read)]
    (cond
      (= opcao 1) (cadastrar-usuario)
      (= opcao 2) (println "Voltando ao menu principal...")
      :else (do (println "Opcao invalida.") (recur)))))

(defn menu-inicial [opcao]
  (cond
    (= opcao 1) (menu-cliente-cadastrado)
    (= opcao 2) (menu-cliente-nao-cadastrado)
    :else (println "\nOpcao invalida... Tente novamente!")))

(defn menu-recursivo []
  (menu-cliente)
  (let [opcao (read)]
    (if (= opcao 3)
      (println "\nAgradecemos pela sua visita!")
      (do
        (menu-inicial opcao)
        (recur)))))

(defn -main []
  (menu-recursivo))