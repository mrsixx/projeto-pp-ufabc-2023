<script>
import { useCorrentistaStore } from '@/stores/CorrentistaStore'

export default {
  setup() {
    const correntistaStore = useCorrentistaStore()
    return {
      correntistaStore,
    }
  },
  props:{
    entry: Object,
  },
  methods: {
    getAmountDisplayClass() {
      const amount = this.getAmount()
      return {
        'text-success': amount >= 0,
        'text-error': amount < 0
      }
    },
    getAmount() {
      if(this.entry.contaOrigemId === this.correntistaStore.contaCorrentePrincipalId)
        return -1 * this.entry.valor
      
      return this.entry.valor
    },
    getFormattedAmount() {
      const amount = this.getAmount()
      return amount.toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' })
    },
    getFormattedDate() {
      return new Date(this.entry.dataOperacao).toLocaleString('pt-BR').replace(',', '')
    },
    getEntryImgByOperationType() {
      switch(this.entry.tipo){
        case "1": return "mdi-piggy-bank-outline";
        case "2": return 'mdi-bank-transfer';
        case "3": return "mdi-barcode-scan";
        default: return "mdi-question-mark-circle-outline";
      }
    },
    getEntryTitleByOperationType() {
      switch(this.entry.tipo){
        case "1": return "Depósito";
        case "2": return "Transferência";
        case "3": return "Pagamento";
        default: return "N/A";
      }
    },
    getColorByOperationType() {
      switch(this.entry.tipo){
        case "1": return "success";
        case "2": return "warning";
        case "3": return "error";
        default: return "info";
      }
    },
  }
}

</script>
<template>
  <VListItem>
    <template #prepend>
      <VAvatar
        start
        rounded
        :color="getColorByOperationType()"
      >
        <v-icon :icon="getEntryImgByOperationType()"/>
      </VAvatar>
    </template>

    <VListItemTitle class="text-sm font-weight-medium mb-1">
      {{ getEntryTitleByOperationType() }}
    </VListItemTitle>
    <VListItemSubtitle class="text-xs">
      {{ getFormattedDate() }}
    </VListItemSubtitle>

    <template #append>
      <VListItemAction class="font-weight-medium" :class="getAmountDisplayClass()">
        {{ getFormattedAmount() }}
      </VListItemAction>
    </template>
  </VListItem>
</template>