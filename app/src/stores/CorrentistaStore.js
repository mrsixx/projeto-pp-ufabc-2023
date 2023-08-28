import { defineStore } from 'pinia'

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
    setSaldo(saldo) {
      this.saldo = parseFloat(saldo)
    },
    setCorrentista(correntista) {
      this.correntista = correntista
    },
    setContasCorrente(contas) {
      this.contaCorrente = contas
    }
  },
  getters: {
    saldoFormatado: (state) => state.saldo.toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' }),
    numConta: (state) => state.contaCorrente.length ? state.contaCorrente[0].numConta : '00000-00',
    nome: (state) => state.correntista.nome,
  }
})