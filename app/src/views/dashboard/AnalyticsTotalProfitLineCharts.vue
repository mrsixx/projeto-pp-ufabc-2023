<script setup>
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import { hexToRgb } from '@layouts/utils'
import VueApexCharts from 'vue3-apexcharts'
import { useTheme } from 'vuetify'

const vuetifyTheme = useTheme()
const currentTheme = controlledComputed(() => vuetifyTheme.name.value, () => vuetifyTheme.current.value.colors)
const variableTheme = controlledComputed(() => vuetifyTheme.name.value, () => vuetifyTheme.current.value.variables)
const correntistaStore = useCorrentistaStore()
const series = [{
  name: 'Entrada',
  color: 'green',
  data: correntistaStore.operacoesEntrada.map(o => o.valor),
},
{
  name: 'Saída',
  color: 'red',
  data: correntistaStore.operacoesSaida.map(o => o.valor),
}]

const chartOptions = controlledComputed(() => vuetifyTheme.name.value, () => {
  return {
    chart: {
      parentHeightOffset: 0,
      toolbar: { show: false },
    },
    tooltip: { enabled: false },
    grid: {
      borderColor: `rgba(${ hexToRgb(String(variableTheme.value['border-color'])) },${ variableTheme.value['border-opacity'] })`,
      strokeDashArray: 6,
      xaxis: { lines: { show: true } },
      yaxis: { lines: { show: false } },
      padding: {
        top: -10,
        left: -7,
        right: 5,
        bottom: 5,
      },
    },
    stroke: {
      width: 3,
      lineCap: 'butt',
      curve: 'straight',
    },
    colors: ['currentTheme.value.primary'],
    markers: {
      size: 6,
      offsetY: 4,
      offsetX: -2,
      strokeWidth: 3,
      colors: ['transparent'],
      strokeColors: 'transparent',
      discrete: [{
        size: 5.5,
        seriesIndex: 0,
        strokeColor: currentTheme.value.primary,
        fillColor: currentTheme.value.surface,
        dataPointIndex: series[0].data.length - 1,
      }],
      hover: { size: 7 },
    },
    xaxis: {
      labels: { show: false },
      axisTicks: { show: false },
      axisBorder: { show: false },
    },
    yaxis: { labels: { show: false } },
  }
})
</script>

<template>
  <VCard>
    <VCardItem>
      <VCardTitle>Movimentação</VCardTitle>

      <template #append>
        <div class="me-n3">
          <MoreBtn />
        </div>
      </template>
    </VCardItem>

    <VCardText>
      <VueApexCharts
        type="line"
        :options="chartOptions"
        :series="series"
        :height="200"
      />
    </VCardText>
  </VCard>
</template>
