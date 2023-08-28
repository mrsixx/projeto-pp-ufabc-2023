<script>
export default {
  props:{
    entry: Object,
  },
  methods: {
    getAmountDisplayClass() {
      return {
        'text-success': this.entry.amount >= 0,
        'text-error': this.entry.amount < 0
      }
    },
    getFormattedAmount() {
      return this.entry.amount.toLocaleString('pt-BR', { style: 'currency', currency: 'BRL' })
    }
  }
}
</script>
<template>
  <VListItem>
    <template #prepend>
      <VAvatar
        start
        rounded
      >
        <VImg
          :height="29"
          :width="28"
          :src="entry.logo"
        />
      </VAvatar>
    </template>

    <VListItemTitle class="text-sm font-weight-medium mb-1">
      {{ entry.title }}
    </VListItemTitle>
    <VListItemSubtitle class="text-xs">
      {{ entry.subtitle }}
    </VListItemSubtitle>

    <template #append>
      <VListItemAction class="font-weight-medium" :class="getAmountDisplayClass()">
        {{ getFormattedAmount() }}
      </VListItemAction>
    </template>
  </VListItem>
</template>