import { defineStore } from 'pinia'
import apiService from '@/services/api-service'

export const useCorrentistaStore = defineStore('CorrentistaStore', {
  state: () => {
    return {
      saldo: 0,
      correntista: {},
      contaCorrente: [],
      operacoes: []
    }
  },
  actions: {
    load(correntistaId) {
      this.loadSaldo(correntistaId)
      this.loadCorrentista(correntistaId)
      this.loadContaCorrente(correntistaId)
      this.loadOperacoes(correntistaId)
    },
    loadSaldo(correntistaId) {
      apiService.carregarSaldo(correntistaId)
        .then(({data}) => this.saldo = parseFloat(data.saldo))
        .catch(x => console.error('Erro ao carregar o saldo', x))
    },
    loadCorrentista(correntistaId) {
      apiService.carregarCorrentista(correntistaId)
        .then(({data}) => this.correntista = data)
        .catch(x => console.error('Erro ao carregar o correntista', x))
    },
    loadContaCorrente(correntistaId) {
      apiService.carregarContaCorrente(correntistaId)
        .then(({data}) => this.contaCorrente = data)
        .catch(x => console.error('Erro ao carregar a conta corrente', x))
    },
    loadOperacoes(correntistaId) {
      apiService.carregarOperacoesCorrentista(correntistaId)
        .then(({data}) => this.operacoes = data)
        .catch(x => console.error('Erro ao carregar operações do correntista.', x))
    }
  },
  getters: {
    saldoFormatado: (state) => state.saldo.toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' }),
    numConta: (state) => state.contaCorrente.length ? state.contaCorrente[0].numConta : '00000-00',
    nome: (state) => state.correntista.nome,
    contaCorrentePrincipalId: (state) => state.loaded ? state.contaCorrente[0].id : 0,
    loaded: (state) => state?.contaCorrente[0] !== undefined,
  }
})