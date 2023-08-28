import axios from 'axios'

class ApiService {
  #httpClient;
  constructor() {
    this.#httpClient = axios.create({
      baseURL: process.env.VUE_API_DOMAIN_URL,
    })
  }
  login(cpf, senha) {
    return this.#httpClient.post('/login', { cpf, senha })
  }
  carregarSaldo(correntistaId) {
    return this.#httpClient.get(`/saldo/${correntistaId}`)
  }
  
  carregarContaCorrente(correntistaId) {
    return this.#httpClient.get(`/conta-corrente-por-correntista/${correntistaId}`)
  }
  carregarCorrentista(correntistaId) {
    return this.#httpClient.get(`/correntista/${correntistaId}`)
  }
  
  carregarOperacoesCorrentista(correntistaId) {
    return this.#httpClient.get(`/operacoes-por-conta-id/${correntistaId}`)
  }

  carregarContaCorrentePorNumConta(numConta) {
    return this.#httpClient.get('/conta-corrente-por-num-conta', { params: { numConta } })
  }

  depositar(contaDestinoId, valor) {
    return this.#httpClient.post(`/operacao-financeira`, {
      valor,
      contaDestinoId,
      dataOperacao: new Date().toISOString(),
      tipo: "1"
    })
  }

  pagar(contaOrigemId, valor) {
    return this.#httpClient.post(`/operacao-financeira`, {
      valor,
      contaOrigemId,
      dataOperacao: new Date().toISOString(),
      tipo: "3"
    })
  }
  
  transferir(contaOrigemId, contaDestinoId, valor) {
    return this.#httpClient.post(`/operacao-financeira`, {
      valor,
      contaDestinoId,
      contaOrigemId,
      dataOperacao: new Date().toISOString(),
      tipo: "2"
    })
  }
}

export default new ApiService()